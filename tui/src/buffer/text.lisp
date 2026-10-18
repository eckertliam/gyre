;;;; text.lisp — writing styled text (spans, lines and texts) into a buffer.
;;;;
;;;; The region functions take X, Y, WIDTH and HEIGHT rather than rects, so widgets can work
;;;; out and draw sub-areas every frame without allocating.

(defpackage #:tui/buffer/text
  (:use #:cl)
  (:import-from #:tui/layout #:rect-x #:rect-y #:rect-width #:rect-height)
  (:import-from #:tui/buffer #:set-string #:set-style-region #:clip-region)
  (:import-from #:tui/text
                #:span-content #:span-style #:line-spans #:line-style #:line-alignment
                #:line-width #:text-lines #:text-style #:text-alignment #:alignment-offset)
  (:import-from #:tui/text/truncate #:line-skip)
  (:export #:set-span
           #:set-line
           #:set-line-range
           #:set-aligned-line
           #:set-text
           #:set-text-region))

(in-package #:tui/buffer/text)

(defun set-span (buffer x y span &key max-width)
  "Write SPAN into row Y of BUFFER from column X in the span's style, as SET-STRING does.
Return the number of columns used."
  (set-string buffer x y (span-content span) :style (span-style span) :max-width max-width))

(declaim (ftype (function (t fixnum fixnum t fixnum fixnum fixnum fixnum fixnum t)
                          (values fixnum &optional))
                set-line-range))
(defun set-line-range (buffer x y line start-span start-char end-span end-char max-width
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

(defun set-line (buffer x y line &key max-width)
  "Write LINE into row Y of BUFFER from column X, each span in the line's style patched
with the span's, stopping at the buffer's edge or after MAX-WIDTH columns. The line's
alignment is ignored; see SET-ALIGNED-LINE. Return the number of columns used."
  (set-line-range buffer x y line 0 0 (length (line-spans line)) 0
                  (or max-width most-positive-fixnum) (line-style line)))

(defun set-aligned-line (buffer x y width line alignment)
  "Draw LINE into the row of WIDTH cells at (X, Y) of BUFFER like ratatui's Line widget: patch
the row with the line's style and write the spans aligned by ALIGNMENT (:left, :center, :right
or NIL for :left). A line wider than the row keeps its left part (:left), its middle (:center)
or its right part (:right). An empty line draws nothing, not even its style."
  (declare (type fixnum x y width))
  (multiple-value-bind (x y width) (clip-region buffer x y width 1)
    (let ((line-width (line-width line))
          (end (length (line-spans line))))
      (when (and (plusp width) (plusp line-width))
        (set-style-region buffer x y width 1 (line-style line))
        (if (<= line-width width)
            (set-line-range buffer (+ x (alignment-offset alignment line-width width)) y line
                              0 0 end 0 width nil)
            (let ((skip (ecase alignment
                          ((nil :left) 0)
                          (:center (floor (- line-width width) 2))
                          (:right (- line-width width)))))
              (multiple-value-bind (span char column) (line-skip line skip)
                ;; A wide character cut by the left edge leaves its right half blank.
                (let ((indent (- column skip)))
                  (set-line-range buffer (+ x indent) y line span char end 0
                                    (- width indent) nil)))))))))

(defun set-text-region (buffer x y width height text &optional (first-line 0))
  "Draw TEXT into the WIDTH × HEIGHT region at (X, Y) of BUFFER like ratatui's Text widget:
patch the region with the text's style, then draw one line per row from the top with
SET-ALIGNED-LINE, each aligned by its own alignment or else the text's. FIRST-LINE is the index of
the line drawn in the top row; the lines above it are skipped."
  (declare (type fixnum x y width height first-line))
  (let ((lines (text-lines text)))
    (declare (type simple-vector lines))
    ;; Rows above the buffer are clipped along with the lines they would show.
    (multiple-value-bind (clipped-x clipped-y width height)
        (clip-region buffer x y width height)
      (set-style-region buffer clipped-x clipped-y width height (text-style text))
      (loop for row of-type fixnum from 0 below height
            for index of-type fixnum from (+ first-line (- clipped-y y)) below (length lines)
            for line = (svref lines index)
            do (set-aligned-line buffer clipped-x (+ clipped-y row) width line
                          (or (line-alignment line) (text-alignment text)))))))

(defun set-text (buffer area text &key (first-line 0))
  "Draw TEXT into the rect AREA of BUFFER like ratatui's Text widget, starting with line
FIRST-LINE; see SET-TEXT-REGION."
  (set-text-region buffer (rect-x area) (rect-y area) (rect-width area) (rect-height area)
                   text first-line))
