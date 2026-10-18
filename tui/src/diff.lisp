;;;; diff.lisp — writing the difference between two buffers as terminal output.
;;;;
;;;; The writer tracks the terminal's cursor and current SGR attributes (the pen) so that it
;;;; moves the cursor only when the next changed cell isn't where the cursor already is and
;;;; emits SGR only when the style changes. Escape sequences are encoded straight into the
;;;; output buffer; nothing here allocates.

(defpackage #:tui/diff
  (:use #:cl)
  (:import-from #:tui/cells
                #:buffer #:buffer-x #:buffer-y #:buffer-width #:buffer-height #:buffer-codes
                #:buffer-fgs #:buffer-bgs #:buffer-underline-colors #:buffer-modifier-sets
                #:buffer-flags #:+continuation+ #:+wide+ #:+uncertain+ #:+skip+
                #:+always-update+ #:cluster-code-p #:cluster-string #:same-grapheme-p)
  (:import-from #:tui/output
                #:output #:output-byte #:output-csi #:output-decimal #:output-char
                #:output-string #:output-fill)
  (:import-from #:tui/style
                #:+reset+ #:+bold+ #:+dim+ #:+italic+ #:+blink+ #:+reverse+ #:+hidden+
                #:+strikethrough+ #:+any-underline+ #:underline-style #:rgb-color-p
                #:color-red #:color-green #:color-blue)
  (:export #:pen
           #:make-pen
           #:reset-pen
           #:pen-x
           #:pen-y
           #:pen-default-p
           #:move-cursor
           #:write-sgr-reset
           #:write-diff))

(in-package #:tui/diff)

(defstruct (pen (:constructor make-pen ()) (:copier nil))
  "What the terminal is known to be doing: the cursor position (-1 when unknown) and the
current SGR colors and modifiers."
  (x -1 :type fixnum)
  (y -1 :type fixnum)
  (fg +reset+ :type fixnum)
  (bg +reset+ :type fixnum)
  (underline-color +reset+ :type fixnum)
  (modifiers 0 :type fixnum))

(defun reset-pen (pen)
  "Forget the cursor position and assume default SGR attributes."
  (setf (pen-x pen) -1 (pen-y pen) -1
        (pen-fg pen) +reset+ (pen-bg pen) +reset+ (pen-underline-color pen) +reset+
        (pen-modifiers pen) 0)
  pen)

(defun pen-default-p (pen)
  "True when PEN has default colors and no modifiers."
  (and (= (pen-fg pen) +reset+) (= (pen-bg pen) +reset+)
       (= (pen-underline-color pen) +reset+) (zerop (pen-modifiers pen))))

;;; Cursor

(defun move-cursor (output pen x y)
  "Move the cursor to column X, row Y (0-based) unless PEN says it is already there."
  (declare (type output output) (type pen pen) (type fixnum x y))
  (unless (and (= x (pen-x pen)) (= y (pen-y pen)))
    (output-csi output)
    (output-decimal output (1+ y))
    (output-byte output 59)
    (output-decimal output (1+ x))
    (output-byte output 72)                     ; H
    (setf (pen-x pen) x (pen-y pen) y)))

;;; SGR

(declaim (inline sgr-parameter))
(defun sgr-parameter (output start number)
  "Append NUMBER as an SGR parameter: after CSI if nothing was written since START, else
after a semicolon."
  (declare (type output output) (type fixnum start number))
  (if (= (output-fill output) start)
      (output-csi output)
      (output-byte output 59))
  (output-decimal output number))

(defun write-color (output start color base)
  "Append the SGR parameters selecting COLOR as foreground (BASE 30) or background (40)."
  (declare (type output output) (type fixnum start color base))
  (cond ((= color +reset+) (sgr-parameter output start (+ base 9)))
        ((rgb-color-p color)
         (sgr-parameter output start (+ base 8))
         (sgr-parameter output start 2)
         (sgr-parameter output start (color-red color))
         (sgr-parameter output start (color-green color))
         (sgr-parameter output start (color-blue color)))
        ((< color 8) (sgr-parameter output start (+ base color)))
        ((< color 16) (sgr-parameter output start (+ base 60 (- color 8))))
        (t (sgr-parameter output start (+ base 8))
           (sgr-parameter output start 5)
           (sgr-parameter output start color))))

(defun write-modifier-changes (output start old new)
  "Append the SGR parameters that turn the modifier set OLD into NEW, underlines aside."
  (declare (type output output) (type fixnum start old new))
  (let ((removed (logandc2 old new))
        (added (logandc2 new old)))
    (declare (type fixnum removed added))
    (when (logtest removed (logior +bold+ +dim+))
      ;; 22 turns off both bold and dim; turn back on whichever stays.
      (sgr-parameter output start 22)
      (setf added (logior added (logand new (logior +bold+ +dim+)))))
    (when (logtest removed +italic+) (sgr-parameter output start 23))
    (when (logtest removed +blink+) (sgr-parameter output start 25))
    (when (logtest removed +reverse+) (sgr-parameter output start 27))
    (when (logtest removed +hidden+) (sgr-parameter output start 28))
    (when (logtest removed +strikethrough+) (sgr-parameter output start 29))
    (when (logtest added +bold+) (sgr-parameter output start 1))
    (when (logtest added +dim+) (sgr-parameter output start 2))
    (when (logtest added +italic+) (sgr-parameter output start 3))
    (when (logtest added +blink+) (sgr-parameter output start 5))
    (when (logtest added +reverse+) (sgr-parameter output start 7))
    (when (logtest added +hidden+) (sgr-parameter output start 8))
    (when (logtest added +strikethrough+) (sgr-parameter output start 9))))

(defun write-underline-style (output modifiers)
  "Append an SGR sequence selecting the underline style MODIFIERS shows."
  (declare (type output output) (type fixnum modifiers))
  (output-csi output)
  (ecase (underline-style modifiers)
    (:none (output-decimal output 24))
    (:single (output-decimal output 4))
    (:double (output-decimal output 21))
    (:curly (output-decimal output 4) (output-byte output 58) (output-decimal output 3))
    (:dotted (output-decimal output 4) (output-byte output 58) (output-decimal output 4))
    (:dashed (output-decimal output 4) (output-byte output 58) (output-decimal output 5)))
  (output-byte output 109))

(defun write-underline-color (output color)
  "Append an SGR sequence selecting the underline color COLOR."
  (declare (type output output) (type fixnum color))
  (flet ((sub (n) (output-byte output 58) (output-decimal output n)))
    (output-csi output)
    (cond ((= color +reset+) (output-decimal output 59))
          ((rgb-color-p color)
           (output-decimal output 58) (sub 2) (output-byte output 58)
           (sub (color-red color)) (sub (color-green color)) (sub (color-blue color)))
          (t (output-decimal output 58) (sub 5) (sub color)))
    (output-byte output 109)))

(defun write-sgr-reset (output pen)
  "Reset the terminal's SGR attributes, unless PEN says they are already the defaults."
  (unless (pen-default-p pen)
    (output-csi output)
    (output-decimal output 0)
    (output-byte output 109)
    (setf (pen-fg pen) +reset+ (pen-bg pen) +reset+ (pen-underline-color pen) +reset+
          (pen-modifiers pen) 0)))

(declaim (inline set-pen))
(defun set-pen (output pen fg bg underline-color modifiers)
  "Make the terminal's SGR attributes FG, BG, UNDERLINE-COLOR and MODIFIERS, emitting only
what differs from PEN."
  (declare (type output output) (type pen pen) (type fixnum fg bg underline-color modifiers))
  (unless (and (= fg (pen-fg pen)) (= bg (pen-bg pen))
               (= underline-color (pen-underline-color pen)) (= modifiers (pen-modifiers pen)))
    (change-pen output pen fg bg underline-color modifiers)))

(defun change-pen (output pen fg bg underline-color modifiers)
  (declare (type output output) (type pen pen) (type fixnum fg bg underline-color modifiers))
  (if (and (= fg +reset+) (= bg +reset+) (= underline-color +reset+) (zerop modifiers))
      (write-sgr-reset output pen)
      (let ((start (output-fill output))
            (old (pen-modifiers pen)))
        (write-modifier-changes output start old modifiers)
        (unless (= fg (pen-fg pen)) (write-color output start fg 30))
        (unless (= bg (pen-bg pen)) (write-color output start bg 40))
        (unless (= start (output-fill output)) (output-byte output 109))
        (unless (eq (underline-style old) (underline-style modifiers))
          (write-underline-style output modifiers))
        (unless (= underline-color (pen-underline-color pen))
          (write-underline-color output underline-color))
        (setf (pen-fg pen) fg (pen-bg pen) bg (pen-underline-color pen) underline-color
              (pen-modifiers pen) modifiers))))

(declaim (inline set-pen-to-cell))
(defun set-pen-to-cell (output pen buffer index)
  (declare (type buffer buffer) (type fixnum index))
  (set-pen output pen
           (aref (buffer-fgs buffer) index)
           (aref (buffer-bgs buffer) index)
           (aref (buffer-underline-colors buffer) index)
           (aref (buffer-modifier-sets buffer) index)))

;;; Cells

(declaim (inline advance))
(defun advance (pen columns right)
  "Account for writing COLUMNS columns at the cursor in a row ending at column RIGHT. At the
right edge the cursor's position becomes unknown (terminals differ on pending wraps)."
  (declare (type pen pen) (type fixnum columns right))
  (let ((x (+ (pen-x pen) columns)))
    (if (or (minusp (pen-x pen)) (>= x right))
        (setf (pen-x pen) -1 (pen-y pen) -1)
        (setf (pen-x pen) x))))

(defun write-grapheme (output buffer code)
  (declare (type buffer buffer) (type (unsigned-byte 32) code))
  (if (cluster-code-p code)
      (output-string output (cluster-string buffer code))
      (output-char output (code-char code))))

(defun write-cell (output pen buffer index x y)
  "Draw cell INDEX of BUFFER, which is at screen column X, row Y."
  (declare (type buffer buffer) (type fixnum index x y) (optimize speed))
  (let* ((flags (aref (buffer-flags buffer) index))
         (wide (logtest flags +wide+))
         (right (+ (buffer-x buffer) (buffer-width buffer))))
    (when (and wide (logtest flags +uncertain+))
      ;; Terminals disagree on the width of this grapheme: clear the column it reserves
      ;; first, so a terminal that draws it wide can't have it overwritten afterwards.
      (move-cursor output pen (1+ x) y)
      (set-pen-to-cell output pen buffer (1+ index))
      (output-byte output 32)
      (advance pen 1 right))
    (move-cursor output pen x y)
    (set-pen-to-cell output pen buffer index)
    (write-grapheme output buffer (aref (buffer-codes buffer) index))
    (if (logtest flags +uncertain+)
        (setf (pen-x pen) -1 (pen-y pen) -1)
        (advance pen (if wide 2 1) right))))

(defmacro with-cell-arrays ((buffer &rest bindings) &body body)
  "Bind each (variable reader) of BINDINGS to the array READER returns for BUFFER, declared
as the array type it is, around BODY."
  (let ((b (gensym "BUFFER")))
    `(let* ((,b ,buffer)
            ,@(loop for (variable reader) in bindings
                    collect `(,variable (,reader ,b))))
       (declare (type (simple-array (unsigned-byte 32) (*))
                      ,@(loop for (variable reader) in bindings
                              unless (member reader '(buffer-modifier-sets buffer-flags))
                                collect variable))
                (type (simple-array (unsigned-byte 16) (*))
                      ,@(loop for (variable reader) in bindings
                              when (eq reader 'buffer-modifier-sets) collect variable))
                (type (simple-array (unsigned-byte 8) (*))
                      ,@(loop for (variable reader) in bindings
                              when (eq reader 'buffer-flags) collect variable)))
       ,@body)))

(defun write-diff (output pen previous next)
  "Append to OUTPUT what turns a terminal showing PREVIOUS into one showing NEXT: every
cell of NEXT that differs from PREVIOUS or is marked :always-update, except cells marked
:skip and the continuation cells of wide graphemes, which are drawn with their grapheme.
The buffers must cover the same area. PEN tracks the terminal's cursor and SGR state;
attributes are left as they are, see WRITE-SGR-RESET."
  (declare (type output output) (type pen pen) (type buffer previous next)
           (optimize speed))
  (assert (= (length (buffer-codes previous)) (length (buffer-codes next))))
  (with-cell-arrays (next (codes buffer-codes) (fgs buffer-fgs) (bgs buffer-bgs)
                          (underlines buffer-underline-colors)
                          (modifiers buffer-modifier-sets) (flags buffer-flags))
    (with-cell-arrays (previous (old-codes buffer-codes) (old-fgs buffer-fgs)
                                (old-bgs buffer-bgs) (old-underlines buffer-underline-colors)
                                (old-modifiers buffer-modifier-sets))
      (let ((width (buffer-width next)) (x (buffer-x next)) (y (buffer-y next)))
        (declare (type (integer 0 #.(floor array-total-size-limit 2)) width)
                 (type fixnum x y))
        (dotimes (row (buffer-height next))
          (let ((base (* row width))
                (column 0))
            (declare (type (integer 0 #.(floor array-total-size-limit 2)) base column))
            (loop while (< column width)
                  do (let* ((index (+ base column))
                            (cell-flags (aref flags index))
                            (code (aref codes index)))
                       (cond ((logtest cell-flags +skip+) (incf column))
                             ((= code +continuation+) (incf column))
                             (t
                              (when (or (logtest cell-flags +always-update+)
                                        (/= (aref fgs index) (aref old-fgs index))
                                        (/= (aref bgs index) (aref old-bgs index))
                                        (/= (aref underlines index) (aref old-underlines index))
                                        (/= (aref modifiers index) (aref old-modifiers index))
                                        (not (same-grapheme-p previous (aref old-codes index)
                                                              next code)))
                                (write-cell output pen next index
                                            (+ column x) (+ row y)))
                              (incf column (if (logtest cell-flags +wide+) 2 1))))))))))))
