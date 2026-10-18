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
  (:import-from #:tui/layout #:make-rect #:rect-x #:rect-y #:rect-width #:rect-height)
  (:import-from #:tui/style
                #:style-fg #:style-bg #:style-underline-color #:apply-modifiers #:+reset+)
  (:import-from #:tui/width
                #:grapheme-end #:grapheme-width #:uncertain-width-p #:do-graphemes)
  (:import-from #:tui/buffer/read
                #:position-outside-buffer #:cell-grapheme #:cell-fg #:cell-bg
                #:cell-underline-color #:cell-modifiers #:cell-wide-p #:cell-continuation-p
                #:cell-diff-option #:buffer-row-strings)
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
           #:set-style-region
           #:clip-region
           #:fill-cells
           #:buffer-row-strings))

(in-package #:tui/buffer)

;;; Conditions

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

(deftype half-index ()
  "A cell index or offset small enough that the sum of two is still an array index."
  `(integer 0 (,(floor array-total-size-limit 2))))

(declaim (inline put-narrow))
(defun put-narrow (buffer index column code base-style style)
  "PUT-GRAPHEME for a single code point one column wide, patched with BASE-STYLE then STYLE."
  (declare (type buffer buffer) (type fixnum index column) (type (unsigned-byte 32) code))
  (let ((codes (buffer-codes buffer))
        (flags (buffer-flags buffer)))
    (when (and (= (aref codes index) +continuation+) (plusp column))
      (blank-cell buffer (1- index)))
    (when (and (logtest (aref flags index) +wide+) (< (1+ column) (buffer-width buffer)))
      (blank-cell buffer (1+ index)))
    (setf (aref codes index) code
          (aref flags index) (logand (aref flags index) +diff-option-flags+))
    (apply-style-at buffer index base-style)
    (apply-style-at buffer index style)))

(declaim (ftype (function (buffer fixnum fixnum simple-string fixnum fixnum fixnum t t)
                          (values fixnum &optional))
                write-graphemes))
(defun write-graphemes (buffer x y string start end limit base-style style)
  "Write the grapheme clusters of STRING from START to END into row Y from column X, up to
column LIMIT, as SET-STRING describes. Return the column after the last one written."
  (declare (optimize speed))
  (let* ((left (buffer-x buffer))
         (row-start (* (- y (buffer-y buffer)) (buffer-width buffer)))
         (column x)
         (i start))
    (declare (type fixnum left column i) (type half-index row-start))
    (loop while (< i end)
          do (let ((code (char-code (schar string i)))
                   (next (1+ i)))
               (declare (type fixnum next))
               (if (and (< #x1F code #x7F)
                        (or (= next end) (< (char-code (schar string next)) #x80)))
                   ;; Printable ASCII followed by ASCII: one cluster, one column.
                   (progn
                     (when (>= column limit) (return))
                     (when (>= column left)
                       (let ((offset (- column left)))
                         (declare (type half-index offset))
                         (put-narrow buffer (+ row-start offset) offset code base-style style)))
                     (setf column (1+ column) i next))
                   (let* ((cluster-end (grapheme-end string i end))
                          (width (grapheme-width string i cluster-end)))
                     (unless (zerop width)
                       (when (> (+ column width) limit) (return))
                       (when (>= column left)
                         (let* ((offset (- column left))
                                (index (+ row-start offset)))
                           (declare (type half-index offset))
                           (put-grapheme buffer index offset
                                         (grapheme-code buffer string i cluster-end) width
                                         (and (= width 2)
                                              (uncertain-width-p string i cluster-end))
                                         base-style)
                           (apply-style-at buffer index style)))
                       (setf column (+ column width)))
                     (setf i cluster-end)))))
    column))

(defun set-string (buffer x y string &key style base-style max-width (start 0) end)
  "Write STRING (from START to END) into row Y of BUFFER starting at column X, one grapheme
cluster per cell (two for wide ones), patching each cell's style with BASE-STYLE and then
STYLE (as if with their PATCH-STYLE, without allocating it). Control characters and other
zero-width clusters are skipped. Writing stops at the buffer's right edge, after MAX-WIDTH
columns, or before a wide cluster that wouldn't fit; columns left of the buffer are clipped.
Return the number of columns used, counted from X."
  (declare (type fixnum x y start))
  (let* ((string (if (simple-string-p string) string (coerce string 'simple-string)))
         (right (buffer-right buffer))
         (limit (if max-width (min right (+ x max-width)) right)))
    (if (cell-index buffer (buffer-x buffer) y) ; the row is inside the buffer
        (- (write-graphemes buffer x y string start (or end (length string)) limit
                            base-style style)
           x)
        0)))

;;; Areas

(declaim (ftype (function (buffer fixnum fixnum fixnum fixnum)
                          (values fixnum fixnum fixnum fixnum &optional))
                clip-region))
(defun clip-region (buffer x y width height)
  "Return the part of the WIDTH × HEIGHT region at (X, Y) that lies inside BUFFER as four
values X, Y, WIDTH and HEIGHT. WIDTH or HEIGHT is 0 when the region misses BUFFER."
  (let ((left (max x (buffer-x buffer)))
        (top (max y (buffer-y buffer)))
        (right (min (+ x (max 0 width)) (buffer-right buffer)))
        (bottom (min (+ y (max 0 height)) (+ (buffer-y buffer) (buffer-height buffer)))))
    (values left top (max 0 (- right left)) (max 0 (- bottom top)))))

(defmacro do-region-cells ((index buffer x y width height) &body body)
  "Evaluate BODY with INDEX bound to each cell index of BUFFER inside the WIDTH × HEIGHT
region at (X, Y)."
  (let ((b (gensym "BUFFER")) (left (gensym "LEFT")) (top (gensym "TOP"))
        (columns (gensym "COLUMNS")) (rows (gensym "ROWS")) (row (gensym "ROW"))
        (column (gensym "COLUMN")))
    `(let ((,b ,buffer))
       (multiple-value-bind (,left ,top ,columns ,rows) (clip-region ,b ,x ,y ,width ,height)
         (declare (type fixnum ,left ,top ,columns ,rows))
         (loop for ,row of-type fixnum from ,top below (+ ,top ,rows)
               do (loop for ,column of-type fixnum from ,left below (+ ,left ,columns)
                        for ,index of-type fixnum = (cell-index ,b ,column ,row)
                        do (progn ,@body)))))))

(defmacro do-area-cells ((index buffer area) &body body)
  "Evaluate BODY with INDEX bound to each cell index of BUFFER inside the rect AREA."
  (let ((a (gensym "AREA")))
    `(let ((,a ,area))
       (do-region-cells (,index ,buffer (rect-x ,a) (rect-y ,a) (rect-width ,a) (rect-height ,a))
         ,@body))))

(defun set-style-region (buffer x y width height style)
  "Patch the style of every cell of BUFFER in the WIDTH × HEIGHT region at (X, Y) with
STYLE, like SET-STYLE without a rect."
  (when style
    (do-region-cells (index buffer x y width height)
      (apply-style-at buffer index style))))

(defun set-style (buffer area style)
  "Patch the style of every cell of BUFFER inside the rect AREA with STYLE."
  (set-style-region buffer (rect-x area) (rect-y area) (rect-width area) (rect-height area)
                    style))

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
      (multiple-value-bind (left top columns rows)
          (clip-region buffer (rect-x area) (rect-y area) (rect-width area) (rect-height area))
        (loop with right = (+ left columns)
              for y from top below (+ top rows)
              do (loop with x = left
                       while (< x right)
                       do (let ((index (cell-index buffer x y))
                                (column (- x (buffer-x buffer))))
                            (reset-cell-style buffer index)
                            (if (<= (+ x width) right)
                                (put-grapheme buffer index column code width uncertain style)
                                (put-grapheme buffer index column +space+ 1 nil style))
                            (incf x width))))))))
