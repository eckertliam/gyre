;;;; paragraph.lisp — the paragraph widget: a text in an optional block, truncated or wrapped,
;;;; scrolled and aligned, as in ratatui.

(defpackage #:tui/widgets/paragraph
  (:use #:cl)
  (:shadowing-import-from #:tui/widgets/block #:block)
  (:import-from #:tui/widgets/block #:block-inner-bounds)
  (:import-from #:tui/style #:style #:patch-style)
  (:import-from #:tui/text #:text-lines #:text-style #:text-alignment #:line-spans #:line-style
                #:line-alignment)
  (:import-from #:tui/text/truncate #:line-truncation #:line-skip)
  (:import-from #:tui/text/wrap #:map-wrapped-lines)
  (:import-from #:tui/widgets/widget #:render)
  (:import-from #:tui/widgets/draw #:clip-area #:as-text)
  (:import-from #:tui/buffer #:set-style-region)
  (:import-from #:tui/buffer/text #:set-line-range)
  (:export #:paragraph
           #:paragraph-line-count))

(in-package #:tui/widgets/paragraph)

(defclass paragraph ()
  ((text :initarg :text :reader paragraph-text)
   (base-styles :initarg :base-styles :reader paragraph-base-styles)
   (block :initarg :block :reader paragraph-block)
   (style :initarg :style :reader paragraph-style)
   (wrap :initarg :wrap :reader paragraph-wrap)
   (trim :initarg :trim :reader paragraph-trim)
   (scroll-x :initarg :scroll-x :reader paragraph-scroll-x)
   (scroll-y :initarg :scroll-y :reader paragraph-scroll-y)
   (alignment :initarg :alignment :reader paragraph-alignment))
  (:documentation "A widget that shows a text, optionally in a block, either truncated to the
width of its area or wrapped, scrolled and aligned. Make one with PARAGRAPH."))

(defun paragraph (text &key block style wrap (trim t) (scroll-x 0) (scroll-y 0) alignment)
  "Return a paragraph widget showing TEXT, a text designator (see MAKE-TEXT).

BLOCK, a block or NIL, is drawn around the text. STYLE patches the whole area, under the
block and the text; the text's, lines' and spans' styles patch only the cells they write.

Without WRAP each line takes one row and is cut at the right edge. With WRAP lines are
wrapped at word boundaries, as by MAP-WRAPPED-LINES; TRIM (true by default) drops the
whitespace at the start of wrapped rows.

SCROLL-Y is the number of rows (after wrapping) skipped at the top, and SCROLL-X the number
of columns skipped at the left of each line. SCROLL-X applies only to left-aligned lines
that aren't wrapped, as in ratatui.

ALIGNMENT (:left, :center or :right) aligns the lines that have no alignment of their own; it
defaults to the text's alignment, and then to :left."
  (check-type block (or null block))
  (check-type style (or null style))
  (check-type scroll-x (and fixnum unsigned-byte))
  (check-type scroll-y (and fixnum unsigned-byte))
  (check-type alignment (member nil :left :center :right))
  (let ((text (as-text text)))
    (make-instance 'paragraph
                   :text text
                   :base-styles (map 'simple-vector
                                     (lambda (line)
                                       (patch-style (text-style text) (line-style line)))
                                     (text-lines text))
                   :block block
                   :style style
                   :wrap (and wrap t)
                   :trim (and trim t)
                   :scroll-x scroll-x
                   :scroll-y scroll-y
                   :alignment (or alignment (text-alignment text) :left))))

(defun paragraph-line-count (paragraph width)
  "Return how many rows PARAGRAPH takes when drawn WIDTH columns wide, counting the rows its
block's borders and titles take, and ignoring its scroll offset. Zero when WIDTH is below 1."
  (if (< width 1)
      0
      (let ((block (paragraph-block paragraph))
            (lines (text-lines (paragraph-text paragraph))))
        (flet ((ignore-row (start-span start-char end-span end-char row-width)
                 (declare (ignore start-span start-char end-span end-char row-width))))
          (+ (if (paragraph-wrap paragraph)
                 (loop for line across lines
                       sum (map-wrapped-lines #'ignore-row line width
                                              :trim (paragraph-trim paragraph)))
                 (length lines))
             (if block
                 ;; The rows the block takes: what it leaves of a region taller than them.
                 (let ((tall (floor most-positive-fixnum 4)))
                   (multiple-value-bind (x y w h) (block-inner-bounds block 0 0 width tall)
                     (declare (ignore x y w))
                     (- tall h)))
                 0))))))

;;; Drawing

(defun line-offset (alignment row-width width)
  "The column at which a row ROW-WIDTH columns wide starts in WIDTH columns, as ratatui's
Paragraph places it: centering rounds the halves of both widths down separately."
  (declare (type fixnum row-width width))
  (ecase alignment
    (:left 0)
    (:center (max 0 (- (floor width 2) (floor row-width 2))))
    (:right (max 0 (- width row-width)))))

(defmethod render ((paragraph paragraph) area buffer)
  (multiple-value-bind (x y width height) (clip-area area buffer)
    (set-style-region buffer x y width height (paragraph-style paragraph))
    (render (paragraph-block paragraph) area buffer)
    (let ((block (paragraph-block paragraph)))
      (when block
        (setf (values x y width height) (block-inner-bounds block x y width height))))
    (when (and (plusp width) (plusp height))
      (set-style-region buffer x y width height (paragraph-style paragraph))
      (if (paragraph-wrap paragraph)
          (draw-wrapped paragraph buffer x y width height)
          (draw-truncated paragraph buffer x y width height)))))

(defun draw-truncated (paragraph buffer x y width height)
  "Draw PARAGRAPH's lines one per row, from line SCROLL-Y, cut to WIDTH columns."
  (declare (type fixnum x y width height))
  (let* ((text (paragraph-text paragraph))
         (lines (text-lines text))
         (base-styles (paragraph-base-styles paragraph))
         (scroll-x (paragraph-scroll-x paragraph))
         (scroll-y (paragraph-scroll-y paragraph)))
    (declare (type simple-vector lines base-styles) (type fixnum scroll-x scroll-y))
    (loop for row of-type fixnum from 0 below height
          for index of-type fixnum from scroll-y below (length lines)
          for line = (svref lines index)
          for alignment = (or (line-alignment line) (paragraph-alignment paragraph))
          do (if (and (eq alignment :left) (plusp scroll-x))
                 (multiple-value-bind (span char column) (line-skip line scroll-x)
                   ;; A wide character cut by the left edge leaves its right half blank.
                   (let ((indent (- column scroll-x)))
                     (when (>= indent 0)  ; otherwise the line is narrower than SCROLL-X
                       (draw-part line (svref base-styles index) buffer (+ x indent) (+ y row)
                                  (- width indent) :left span char))))
                 (draw-part line (svref base-styles index) buffer x (+ y row) width alignment
                            0 0)))))

(defun draw-part (line base-style buffer x y width alignment start-span start-char)
  "Draw the part of LINE from position (START-SPAN, START-CHAR) that fits in WIDTH columns,
aligned by ALIGNMENT, in row Y from column X."
  (declare (type fixnum x y width))
  (when (plusp width)
    (multiple-value-bind (end-span end-char row-width)
        (line-truncation line width :start-span start-span :start-char start-char)
      (set-line-range buffer (+ x (line-offset alignment row-width width)) y line
                        start-span start-char end-span end-char width base-style))))

(defun draw-wrapped (paragraph buffer x y width height)
  "Draw PARAGRAPH's lines wrapped to WIDTH columns, skipping the first SCROLL-Y rows."
  (declare (type fixnum x y width height))
  (let* ((text (paragraph-text paragraph))
         (lines (text-lines text))
         (base-styles (paragraph-base-styles paragraph))
         (skip (paragraph-scroll-y paragraph))
         (row 0))
    (declare (type simple-vector lines base-styles) (type fixnum skip row))
    (cl:block draw
      (loop for index of-type fixnum from 0 below (length lines)
            for line = (svref lines index)
            for base-style = (svref base-styles index)
            for alignment = (or (line-alignment line) (paragraph-alignment paragraph))
            do (flet ((draw-row (start-span start-char end-span end-char row-width)
                        (declare (type fixnum row-width))
                        (cond ((plusp skip) (decf skip))
                              ((< row height)
                               (set-line-range buffer
                                                 (+ x (line-offset alignment row-width width))
                                                 (+ y row) line start-span start-char
                                                 end-span end-char width base-style)
                               (incf row))
                              (t (return-from draw)))))
                 (declare (dynamic-extent #'draw-row))
                 (map-wrapped-lines #'draw-row line width :trim (paragraph-trim paragraph)))))))
