;;;; tabs.lisp — the tabs widget: a row of titles with one highlighted, as in ratatui.

(defpackage #:tui/widgets/tabs
  (:use #:cl)
  (:shadowing-import-from #:tui/widgets/block #:block)
  (:import-from #:tui/widgets/block #:block-inner-bounds)
  (:import-from #:tui/style #:style #:make-style)
  (:import-from #:tui/text #:span)
  (:import-from #:tui/buffer/text #:set-line #:set-span)
  (:import-from #:tui/widgets/widget #:render)
  (:import-from #:tui/widgets/draw #:clip-area #:as-line)
  (:import-from #:tui/buffer #:set-style-region)
  (:export #:tabs))

(in-package #:tui/widgets/tabs)

(defclass tabs ()
  ((titles :initarg :titles :reader tabs-titles)
   (selected :initarg :selected :reader tabs-selected)
   (block :initarg :block :reader tabs-block)
   (style :initarg :style :reader tabs-style)
   (highlight-style :initarg :highlight-style :reader tabs-highlight-style)
   (divider :initarg :divider :reader tabs-divider)
   (padding-left :initarg :padding-left :reader tabs-padding-left)
   (padding-right :initarg :padding-right :reader tabs-padding-right))
  (:documentation "A widget that shows titles in a row, separated by dividers, with the
selected one highlighted. Make one with TABS."))

(defun tabs (titles &key block style (highlight-style (make-style :modifiers :reverse))
                      (selected 0) (divider (string (code-char #x2502))) (padding-left " ")
                      (padding-right " "))
  "Return a tabs widget showing TITLES, a list or vector of line designators (see MAKE-LINE),
on the first row of its area.

Each title is drawn between PADDING-LEFT and PADDING-RIGHT, line designators that default to
one space, and titles are separated by DIVIDER, a string or span that defaults to a vertical
line. The title at index SELECTED (0 by default; NIL for none) is patched with
HIGHLIGHT-STYLE, which defaults to reversed colors. Whatever doesn't fit is cut off at the
right edge.

BLOCK, a block or NIL, is drawn around the tabs. STYLE patches the whole area."
  (check-type titles sequence)
  (check-type block (or null block))
  (check-type style (or null style))
  (check-type highlight-style (or null style))
  (check-type selected (or null (and fixnum unsigned-byte)))
  (check-type divider (or string span))
  (make-instance 'tabs
                 :titles (map 'simple-vector #'as-line titles)
                 :selected selected
                 :block block
                 :style style
                 :highlight-style highlight-style
                 :divider (if (stringp divider) (span divider) divider)
                 :padding-left (as-line padding-left)
                 :padding-right (as-line padding-right)))

(defmethod render ((tabs tabs) area buffer)
  (multiple-value-bind (x y width height) (clip-area area buffer)
    (when (and (plusp width) (plusp height))
      (set-style-region buffer x y width height (tabs-style tabs))
      (render (tabs-block tabs) area buffer)
      (when (tabs-block tabs)
        (setf (values x y width height) (block-inner-bounds (tabs-block tabs) x y width height)))
      (when (and (plusp width) (plusp height))
        (draw-tabs tabs buffer x y width)))))

(defun draw-tabs (tabs buffer x y width)
  "Draw the titles, paddings and dividers from (X, Y), in at most WIDTH columns."
  (declare (type fixnum x y width))
  (let ((right (+ x width))
        (titles (tabs-titles tabs)))
    (declare (type fixnum right) (type simple-vector titles))
    (loop for index of-type fixnum from 0 below (length titles)
          do (when (>= x right) (return))
             (incf x (set-line buffer x y (tabs-padding-left tabs) :max-width (- right x)))
             (when (>= x right) (return))
             (let ((used (set-line buffer x y (svref titles index) :max-width (- right x))))
               (declare (type fixnum used))
               (when (eql index (tabs-selected tabs))
                 (set-style-region buffer x y used 1 (tabs-highlight-style tabs)))
               (incf x used))
             (when (>= x right) (return))
             (incf x (set-line buffer x y (tabs-padding-right tabs) :max-width (- right x)))
             (when (or (>= x right) (= index (1- (length titles))))
               (return))
             (incf x (set-span buffer x y (tabs-divider tabs) :max-width (- right x))))))
