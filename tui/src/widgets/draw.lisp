;;;; draw.lisp — drawing helpers shared by the built-in widgets. Not part of the public API.
;;;;
;;;; They take regions as X, Y, WIDTH and HEIGHT rather than as rects, so that widgets can work
;;;; out and draw sub-areas every frame without allocating rects.

(defpackage #:tui/widgets/draw
  (:use #:cl)
  (:import-from #:tui/cells #:buffer-x #:buffer-y #:buffer-width #:buffer-height)
  (:import-from #:tui/layout #:rect-x #:rect-y #:rect-width #:rect-height)
  (:import-from #:tui/buffer #:set-string #:set-cell-style)
  (:import-from #:tui/text
                #:span-content #:span-style #:line #:make-line #:line-spans #:line-style
                #:line-alignment #:line-width #:text #:make-text #:text-lines #:text-style
                #:text-alignment #:alignment-offset)
  (:import-from #:tui/text/truncate #:line-skip)
  (:export #:clip-region
           #:clip-area
           #:style-region
           #:write-line-range
           #:draw-line
           #:draw-text
           #:as-text
           #:as-line
           #:highlight-spacing
           #:selection-column-p))

(in-package #:tui/widgets/draw)

;;; Regions

(declaim (ftype (function (t fixnum fixnum fixnum fixnum)
                          (values fixnum fixnum fixnum fixnum &optional))
                clip-region))
(defun clip-region (buffer x y width height)
  "Return the part of the WIDTH × HEIGHT region at (X, Y) that lies inside BUFFER, as four
values X, Y, WIDTH and HEIGHT. WIDTH or HEIGHT is 0 when the region misses BUFFER."
  (let ((left (max x (buffer-x buffer)))
        (top (max y (buffer-y buffer)))
        (right (min (+ x (max 0 width)) (+ (buffer-x buffer) (buffer-width buffer))))
        (bottom (min (+ y (max 0 height)) (+ (buffer-y buffer) (buffer-height buffer)))))
    (values left top (max 0 (- right left)) (max 0 (- bottom top)))))

(defun clip-area (area buffer)
  "Return the part of the rect AREA that lies inside BUFFER as four values, as CLIP-REGION."
  (clip-region buffer (rect-x area) (rect-y area) (rect-width area) (rect-height area)))

(defun style-region (buffer x y width height style)
  "Patch the cells of BUFFER in the WIDTH × HEIGHT region at (X, Y) with STYLE (or NIL), as
SET-STYLE does."
  (declare (type fixnum x y width height))
  (when style
    ;; Cell by cell: SET-STYLE would need a rect, and MAKE-RECT can't be stack-allocated
    ;; because the structure's constructor isn't inline.
    (multiple-value-bind (x y width height) (clip-region buffer x y width height)
      (loop for row of-type fixnum from y below (+ y height)
            do (loop for column of-type fixnum from x below (+ x width)
                     do (set-cell-style buffer column row style))))))

;;; Lines and texts

(declaim (ftype (function (t fixnum fixnum t fixnum fixnum fixnum fixnum fixnum t)
                          (values fixnum &optional))
                write-line-range))
(defun write-line-range (buffer x y line start-span start-char end-span end-char max-width
                         base-style)
  "Write the characters of LINE from position (START-SPAN, START-CHAR) up to (END-SPAN,
END-CHAR) into row Y of BUFFER from column X, in at most MAX-WIDTH columns, each span in
BASE-STYLE (or NIL) patched with its own style, as SET-STRING writes. A position past the
last span means the end of the line. Return the number of columns used."
  (let ((spans (line-spans line))
        (used 0))
    (declare (type fixnum used))
    (loop for index of-type fixnum from start-span to (min end-span (1- (length spans)))
          for span = (svref spans index)
          for from of-type fixnum = (if (= index start-span) start-char 0)
          for to of-type fixnum = (if (= index end-span) end-char (length (span-content span)))
          while (< used max-width)
          when (< from to)
            do (incf used (set-string buffer (+ x used) y (span-content span)
                                      :base-style base-style :style (span-style span)
                                      :max-width (- max-width used) :start from :end to)))
    used))

(defun draw-line (buffer x y width line alignment)
  "Draw LINE into the row of WIDTH cells at (X, Y) of BUFFER like ratatui's Line widget: patch
the row with the line's style and write the spans aligned by ALIGNMENT (:left, :center, :right
or NIL for :left). A line wider than the row keeps its left part (:left), its middle (:center)
or its right part (:right). An empty line draws nothing, not even its style."
  (declare (type fixnum x y width))
  (multiple-value-bind (x y width) (clip-region buffer x y width 1)
    (let ((line-width (line-width line))
          (end (length (line-spans line))))
      (when (and (plusp width) (plusp line-width))
        (style-region buffer x y width 1 (line-style line))
        (if (<= line-width width)
            (write-line-range buffer (+ x (alignment-offset alignment line-width width)) y line
                              0 0 end 0 width nil)
            (let ((skip (ecase alignment
                          ((nil :left) 0)
                          (:center (floor (- line-width width) 2))
                          (:right (- line-width width)))))
              (multiple-value-bind (span char column) (line-skip line skip)
                ;; A wide character cut by the left edge leaves its right half blank.
                (let ((indent (- column skip)))
                  (write-line-range buffer (+ x indent) y line span char end 0
                                    (- width indent) nil)))))))))

(defun as-text (designator)
  "Return the text DESIGNATOR designates (see MAKE-TEXT); a text is returned as it is, keeping
its style and alignment for the whole text rather than folding them into its lines."
  (if (typep designator 'text) designator (make-text designator)))

(defun as-line (designator)
  "Return the line DESIGNATOR designates (see MAKE-LINE); a line is returned as it is."
  (if (typep designator 'line) designator (make-line designator)))

(defun draw-text (buffer x y width height text &optional (first-line 0))
  "Draw TEXT into the WIDTH × HEIGHT region at (X, Y) of BUFFER like ratatui's Text widget:
patch the region with the text's style, then draw one line per row from the top with
DRAW-LINE, each aligned by its own alignment or else the text's. FIRST-LINE is the index of
the line drawn in the top row; the lines above it are skipped."
  (declare (type fixnum x y width height first-line))
  (let ((lines (text-lines text)))
    (declare (type simple-vector lines))
    ;; Rows above the buffer are clipped along with the lines they would show.
    (multiple-value-bind (clipped-x clipped-y width height)
        (clip-region buffer x y width height)
      (style-region buffer clipped-x clipped-y width height (text-style text))
      (loop for row of-type fixnum from 0 below height
            for index of-type fixnum from (+ first-line (- clipped-y y)) below (length lines)
            for line = (svref lines index)
            do (draw-line buffer clipped-x (+ clipped-y row) width line
                          (or (line-alignment line) (text-alignment text)))))))

;;; Highlight symbols

(deftype highlight-spacing ()
  "When a list or table reserves the column for its highlight symbol: :always, :when-selected
(only while an item is selected) or :never."
  '(member :always :when-selected :never))

(defun selection-column-p (spacing selectedp)
  "True when a widget with highlight spacing SPACING reserves the column for its highlight
symbol, given whether an item is selected (SELECTEDP)."
  (ecase spacing
    (:always t)
    (:when-selected selectedp)
    (:never nil)))
