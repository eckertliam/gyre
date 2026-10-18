;;;; buffer.lisp — a rectangular grid of styled cells that widgets draw into.
;;;;
;;;; Cells live in specialized arrays (see tui/cells), so an ordinary single-code-point cell
;;;; holds no heap object. A grapheme two columns wide occupies its cell and a continuation
;;;; cell to its right; writing over either half clears the other.

(defpackage #:tui/buffer
  (:use #:cl)
  (:import-from #:tui/cells
                #:buffer #:bufferp #:%make-buffer #:buffer-x #:buffer-y #:buffer-width
                #:buffer-height #:buffer-codes #:buffer-fgs #:buffer-bgs
                #:buffer-underline-colors #:buffer-modifier-sets #:buffer-flags
                #:buffer-clusters #:allocate-cells #:clear-cells #:+space+ #:+continuation+
                #:+cluster-base+ #:+wide+ #:+uncertain+ #:+skip+ #:+always-update+
                #:+diff-option-flags+ #:cluster-code-p #:cluster-string #:intern-cluster
                #:cluster-count #:clear-clusters)
  (:import-from #:tui/layout
                #:make-rect #:rect-x #:rect-y #:rect-width #:rect-height #:rect-right
                #:rect-bottom)
  (:import-from #:tui/style
                #:style-fg #:style-bg #:style-underline-color #:apply-modifiers #:+reset+)
  (:import-from #:tui/width
                #:grapheme-end #:grapheme-width #:uncertain-width-p #:do-graphemes)
  (:export #:buffer
           #:bufferp
           #:make-buffer
           #:buffer-area
           #:resize-buffer
           #:reset-buffer
           #:position-outside-buffer
           #:invalid-grapheme
           #:cell-grapheme
           #:cell-fg
           #:cell-bg
           #:cell-underline-color
           #:cell-modifiers
           #:cell-wide-p
           #:cell-continuation-p
           #:cell-diff-option
           #:set-cell
           #:set-cell-style
           #:set-diff-option
           #:set-string
           #:set-style
           #:fill-cells
           #:buffer-row-strings))

(in-package #:tui/buffer)

;;; Conditions

(define-condition position-outside-buffer (error)
  ((x :initarg :x :reader position-outside-buffer-x)
   (y :initarg :y :reader position-outside-buffer-y))
  (:report (lambda (condition stream)
             (format stream "Position (~D, ~D) is outside the buffer."
                     (position-outside-buffer-x condition)
                     (position-outside-buffer-y condition))))
  (:documentation "Signaled when reading a cell outside a buffer's area."))

(define-condition invalid-grapheme (error)
  ((grapheme :initarg :grapheme :reader invalid-grapheme-grapheme))
  (:report (lambda (condition stream)
             (format stream "~S is not a single grapheme one or two columns wide."
                     (invalid-grapheme-grapheme condition))))
  (:documentation "Signaled when a cell is given something other than one printable
grapheme cluster."))

;;; Creating and resizing

(defun make-buffer (area)
  "Return a buffer covering the rect AREA, with every cell empty: a space with the
terminal's default colors and no modifiers."
  (let ((buffer (%make-buffer 0 0 0 0)))
    (allocate-cells buffer (rect-x area) (rect-y area) (rect-width area) (rect-height area))))

(defun buffer-area (buffer)
  "Return the rect BUFFER covers."
  (make-rect (buffer-x buffer) (buffer-y buffer) (buffer-width buffer) (buffer-height buffer)))

(defun resize-buffer (buffer area)
  "Make BUFFER cover the rect AREA, with every cell empty."
  (allocate-cells buffer (rect-x area) (rect-y area) (rect-width area) (rect-height area)))

(defparameter *cluster-table-limit* 4096
  "RESET-BUFFER empties a buffer's table of multi-code-point graphemes once it holds more
than this many, bounding its memory. Below the limit, entries are kept so that drawing the
same graphemes again doesn't allocate.")

(defun reset-buffer (buffer)
  "Make every cell of BUFFER empty."
  (when (> (cluster-count (buffer-clusters buffer)) *cluster-table-limit*)
    (clear-clusters (buffer-clusters buffer)))
  (clear-cells buffer 0 (length (buffer-codes buffer))))

;;; Cell positions

(declaim (inline buffer-right))
(defun buffer-right (buffer)
  (+ (buffer-x buffer) (buffer-width buffer)))

(declaim (inline cell-index))
(defun cell-index (buffer x y)
  "The index of cell (X, Y) in BUFFER's arrays, or NIL when it's outside the buffer."
  (declare (type buffer buffer) (type fixnum x y))
  (let ((column (- x (buffer-x buffer)))
        (row (- y (buffer-y buffer))))
    (and (< -1 column (buffer-width buffer))
         (< -1 row (buffer-height buffer))
         (+ (* row (buffer-width buffer)) column))))

(defun checked-index (buffer x y)
  (or (cell-index buffer x y)
      (error 'position-outside-buffer :x x :y y)))

;;; Reading cells

(defun cell-grapheme (buffer x y)
  "Return the grapheme in cell (X, Y) of BUFFER as a fresh string; the empty string for the
right half of a wide grapheme."
  (let ((code (aref (buffer-codes buffer) (checked-index buffer x y))))
    (cond ((= code +continuation+) "")
          ((cluster-code-p code) (copy-seq (cluster-string buffer code)))
          (t (string (code-char code))))))

(defun cell-fg (buffer x y)
  "The foreground color of cell (X, Y) of BUFFER."
  (aref (buffer-fgs buffer) (checked-index buffer x y)))

(defun cell-bg (buffer x y)
  "The background color of cell (X, Y) of BUFFER."
  (aref (buffer-bgs buffer) (checked-index buffer x y)))

(defun cell-underline-color (buffer x y)
  "The underline color of cell (X, Y) of BUFFER."
  (aref (buffer-underline-colors buffer) (checked-index buffer x y)))

(defun cell-modifiers (buffer x y)
  "The modifier set of cell (X, Y) of BUFFER."
  (aref (buffer-modifier-sets buffer) (checked-index buffer x y)))

(defun cell-wide-p (buffer x y)
  "True when cell (X, Y) of BUFFER holds a grapheme two columns wide."
  (logtest +wide+ (aref (buffer-flags buffer) (checked-index buffer x y))))

(defun cell-continuation-p (buffer x y)
  "True when cell (X, Y) of BUFFER is covered by the right half of a wide grapheme."
  (= +continuation+ (aref (buffer-codes buffer) (checked-index buffer x y))))

(defun cell-diff-option (buffer x y)
  "How the diff treats cell (X, Y) of BUFFER: :none, :skip or :always-update."
  (let ((flags (aref (buffer-flags buffer) (checked-index buffer x y))))
    (cond ((logtest flags +skip+) :skip)
          ((logtest flags +always-update+) :always-update)
          (t :none))))

;;; Writing cells

(declaim (inline copy-cell-style))
(defun copy-cell-style (buffer from to)
  (declare (type buffer buffer) (type fixnum from to))
  (setf (aref (buffer-fgs buffer) to) (aref (buffer-fgs buffer) from)
        (aref (buffer-bgs buffer) to) (aref (buffer-bgs buffer) from)
        (aref (buffer-underline-colors buffer) to) (aref (buffer-underline-colors buffer) from)
        (aref (buffer-modifier-sets buffer) to) (aref (buffer-modifier-sets buffer) from)))

(declaim (inline apply-style-at))
(defun apply-style-at (buffer index style)
  "Patch cell INDEX of BUFFER with STYLE: set the colors STYLE sets and add and remove its
modifiers. The right half of a wide grapheme always has the style of its left half, which
the terminal draws across both columns: patching a wide grapheme patches both halves and
patching a continuation cell alone does nothing."
  (declare (type buffer buffer) (type fixnum index))
  (when (and style (/= (aref (buffer-codes buffer) index) +continuation+))
    (let ((fg (style-fg style))
          (bg (style-bg style))
          (underline (style-underline-color style)))
      (when fg (setf (aref (buffer-fgs buffer) index) fg))
      (when bg (setf (aref (buffer-bgs buffer) index) bg))
      (when underline (setf (aref (buffer-underline-colors buffer) index) underline))
      (setf (aref (buffer-modifier-sets buffer) index)
            (apply-modifiers (aref (buffer-modifier-sets buffer) index) style))
      (when (logtest (aref (buffer-flags buffer) index) +wide+)
        (copy-cell-style buffer index (1+ index))))))


(declaim (inline blank-cell))
(defun blank-cell (buffer index)
  "Replace the grapheme of cell INDEX with a space, keeping its style and diff option."
  (declare (type buffer buffer) (type fixnum index))
  (setf (aref (buffer-codes buffer) index) +space+
        (aref (buffer-flags buffer) index)
        (logand (aref (buffer-flags buffer) index) +diff-option-flags+)))

(defun clear-overlapped (buffer index column width)
  "Before a grapheme WIDTH columns wide goes into cell INDEX (at COLUMN of its row), blank
the halves of wide graphemes it would cut in two."
  (declare (type buffer buffer) (type fixnum index column width))
  (let ((codes (buffer-codes buffer))
        (flags (buffer-flags buffer)))
    (when (and (= (aref codes index) +continuation+) (plusp column))
      (blank-cell buffer (1- index)))
    (let ((last (+ index width -1)))
      (when (and (logtest (aref flags last) +wide+)
                 (< (+ column width) (buffer-width buffer)))
        (blank-cell buffer (1+ last))))))

(defun put-grapheme (buffer index column code width uncertain style)
  "Store the grapheme CODE, WIDTH columns wide, in cell INDEX at COLUMN and patch its style
with STYLE. A wide grapheme also takes the next cell as its continuation."
  (declare (type buffer buffer) (type fixnum index column width)
           (type (unsigned-byte 32) code))
  (clear-overlapped buffer index column width)
  (let ((flags (buffer-flags buffer)))
    (setf (aref (buffer-codes buffer) index) code
          (aref flags index) (logior (logand (aref flags index) +diff-option-flags+)
                                     (if (= width 2) +wide+ 0)
                                     (if uncertain +uncertain+ 0)))
    (apply-style-at buffer index style)
    (when (= width 2)
      (let ((next (1+ index)))
        (setf (aref (buffer-codes buffer) next) +continuation+
              (aref flags next) (logand (aref flags next) +diff-option-flags+))
        (copy-cell-style buffer index next)))))

(defun grapheme-code (buffer string start end)
  "The code to store for the grapheme STRING[START,END) in BUFFER."
  (declare (type simple-string string) (type fixnum start end))
  (if (= end (1+ start))
      (char-code (schar string start))
      (+ +cluster-base+ (intern-cluster (buffer-clusters buffer) string start end))))

(defun grapheme-designator (grapheme)
  "Return GRAPHEME as a simple string after checking that it is one printable grapheme
cluster, and its width."
  (let* ((string (etypecase grapheme
                   (character (string grapheme))
                   (string (coerce grapheme 'simple-string))))
         (end (length string)))
    (unless (and (plusp end) (= end (grapheme-end string 0 end)))
      (error 'invalid-grapheme :grapheme grapheme))
    (let ((width (grapheme-width string 0 end)))
      (when (zerop width)
        (error 'invalid-grapheme :grapheme grapheme))
      (values string width))))

(defun set-cell (buffer x y grapheme &optional style)
  "Put GRAPHEME, a character or a string holding one grapheme cluster one or two columns
wide, in cell (X, Y) of BUFFER and patch the cell's style with STYLE. Signal
INVALID-GRAPHEME for anything else. Positions outside the buffer, and a wide grapheme that
doesn't fit before the right edge, are ignored."
  (multiple-value-bind (string width) (grapheme-designator grapheme)
    (let ((index (cell-index buffer x y))
          (column (- x (buffer-x buffer))))
      (when (and index (<= (+ column width) (buffer-width buffer)))
        (put-grapheme buffer index column (grapheme-code buffer string 0 (length string))
                      width (uncertain-width-p string 0 (length string)) style)))))

(defun set-cell-style (buffer x y style)
  "Patch the style of cell (X, Y) of BUFFER with STYLE. Positions outside are ignored."
  (let ((index (cell-index buffer x y)))
    (when index
      (apply-style-at buffer index style))))

;;; Strings

(defun set-string (buffer x y string &key style max-width (start 0) end)
  "Write STRING (from START to END) into row Y of BUFFER starting at column X, one grapheme
cluster per cell (two for wide ones), patching each cell's style with STYLE. Control
characters and other zero-width clusters are skipped. Writing stops at the buffer's right
edge, after MAX-WIDTH columns, or before a wide cluster that wouldn't fit; columns left of
the buffer are clipped. Return the number of columns used, counted from X."
  (declare (type fixnum x y start))
  (let* ((string (if (simple-string-p string) string (coerce string 'simple-string)))
         (right (buffer-right buffer))
         (limit (if max-width (min right (+ x max-width)) right))
         (column x))
    (declare (type simple-string string) (type fixnum right limit column))
    (when (cell-index buffer (buffer-x buffer) y) ; the row is inside the buffer
      (do-graphemes ((s e width) string :start start :to end)
        (unless (zerop width)
          (when (> (+ column width) limit)
            (return))
          (when (>= column (buffer-x buffer))
            (put-grapheme buffer (cell-index buffer column y) (- column (buffer-x buffer))
                          (grapheme-code buffer string s e) width
                          (and (= width 2) (uncertain-width-p string s e)) style))
          (incf column width))))
    (- column x)))

;;; Areas

(defun clip (buffer area)
  "The rows and columns of AREA inside BUFFER: four values, left right top bottom."
  (values (max (rect-x area) (buffer-x buffer))
          (min (rect-right area) (buffer-right buffer))
          (max (rect-y area) (buffer-y buffer))
          (min (rect-bottom area) (+ (buffer-y buffer) (buffer-height buffer)))))

(defmacro do-area-cells ((index buffer area) &body body)
  "Evaluate BODY with INDEX bound to each cell index of BUFFER inside the rect AREA."
  (let ((b (gensym "BUFFER")) (left (gensym "LEFT")) (right (gensym "RIGHT"))
        (top (gensym "TOP")) (bottom (gensym "BOTTOM")) (y (gensym "Y")) (x (gensym "X")))
    `(let ((,b ,buffer))
       (multiple-value-bind (,left ,right ,top ,bottom) (clip ,b ,area)
         (declare (type fixnum ,left ,right ,top ,bottom))
         (loop for ,y of-type fixnum from ,top below ,bottom
               do (loop for ,x of-type fixnum from ,left below ,right
                        for ,index of-type fixnum = (cell-index ,b ,x ,y)
                        do (progn ,@body)))))))

(defun set-style (buffer area style)
  "Patch the style of every cell of BUFFER inside the rect AREA with STYLE."
  (do-area-cells (index buffer area)
    (apply-style-at buffer index style)))

(defun set-diff-option (buffer area option)
  "Set how the diff treats the cells of BUFFER inside AREA: :none (draw when changed),
:skip (never draw, for areas another renderer owns, such as images or an embedded
terminal) or :always-update (draw every frame)."
  (let ((bits (ecase option
                (:none 0)
                (:skip +skip+)
                (:always-update +always-update+)))
        (flags (buffer-flags buffer)))
    (do-area-cells (index buffer area)
      (setf (aref flags index)
            (logior (logandc2 (aref flags index) +diff-option-flags+) bits)))))

(defun reset-cell-style (buffer index)
  (setf (aref (buffer-fgs buffer) index) +reset+
        (aref (buffer-bgs buffer) index) +reset+
        (aref (buffer-underline-colors buffer) index) +reset+
        (aref (buffer-modifier-sets buffer) index) 0))

(defun fill-cells (buffer area grapheme &optional style)
  "Replace every cell of BUFFER inside AREA with GRAPHEME (see SET-CELL) in default colors
patched with STYLE. A wide GRAPHEME goes in every other column; a column left over at the
right edge becomes a space."
  (multiple-value-bind (string width) (grapheme-designator grapheme)
    (let ((code (grapheme-code buffer string 0 (length string)))
          (uncertain (uncertain-width-p string 0 (length string))))
      (multiple-value-bind (left right top bottom) (clip buffer area)
        (loop for y from top below bottom
              do (loop with x = left
                       while (< x right)
                       do (let ((index (cell-index buffer x y))
                                (column (- x (buffer-x buffer))))
                            (reset-cell-style buffer index)
                            (if (<= (+ x width) right)
                                (put-grapheme buffer index column code width uncertain style)
                                (put-grapheme buffer index column +space+ 1 nil style))
                            (incf x width))))))))

;;; Inspecting

(defun buffer-row-strings (buffer)
  "Return the graphemes of BUFFER as a list of strings, one per row, leaving out the
continuation cells of wide graphemes. For tests and debugging."
  (loop for y from (buffer-y buffer) below (+ (buffer-y buffer) (buffer-height buffer))
        collect (with-output-to-string (out)
                  (loop for x from (buffer-x buffer) below (buffer-right buffer)
                        do (write-string (cell-grapheme buffer x y) out)))))
