;;;; read.lisp — reading the cells of a buffer.

(defpackage #:tui/buffer/read
  (:use #:cl)
  (:import-from #:tui/cells
                #:buffer #:buffer-x #:buffer-y #:buffer-width #:buffer-height #:buffer-codes
                #:buffer-fgs #:buffer-bgs #:buffer-underline-colors #:buffer-modifier-sets
                #:buffer-flags #:+continuation+ #:+wide+ #:+skip+ #:+always-update+
                #:cluster-code-p #:cluster-string)
  (:export #:position-outside-buffer
           #:cell-grapheme
           #:cell-fg
           #:cell-bg
           #:cell-underline-color
           #:cell-modifiers
           #:cell-wide-p
           #:cell-continuation-p
           #:cell-diff-option
           #:buffer-row-strings))

(in-package #:tui/buffer/read)

(define-condition position-outside-buffer (error)
  ((x :initarg :x :reader position-outside-buffer-x)
   (y :initarg :y :reader position-outside-buffer-y))
  (:report (lambda (condition stream)
             (format stream "Position (~D, ~D) is outside the buffer."
                     (position-outside-buffer-x condition)
                     (position-outside-buffer-y condition))))
  (:documentation "Signaled when reading a cell outside a buffer's area."))

(defun checked-index (buffer x y)
  "The index of cell (X, Y) in BUFFER's arrays; signal POSITION-OUTSIDE-BUFFER when it's
outside the buffer."
  (let ((column (- x (buffer-x buffer)))
        (row (- y (buffer-y buffer))))
    (unless (and (< -1 column (buffer-width buffer)) (< -1 row (buffer-height buffer)))
      (error 'position-outside-buffer :x x :y y))
    (+ (* row (buffer-width buffer)) column)))

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

;;; Inspecting

(defun buffer-row-strings (buffer)
  "Return the graphemes of BUFFER as a list of strings, one per row, leaving out the
continuation cells of wide graphemes. For tests and debugging."
  (loop for y from (buffer-y buffer) below (+ (buffer-y buffer) (buffer-height buffer))
        collect (with-output-to-string (out)
                  (loop for x from (buffer-x buffer) below (+ (buffer-x buffer) (buffer-width buffer))
                        do (write-string (cell-grapheme buffer x y) out)))))
