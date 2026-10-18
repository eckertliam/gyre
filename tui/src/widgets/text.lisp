;;;; text.lisp — strings, spans, lines and texts as widgets, drawn as ratatui draws &str, Span,
;;;; Line and Text.

(defpackage #:tui/widgets/text
  (:use #:cl)
  (:import-from #:tui/buffer #:set-string)
  (:import-from #:tui/text #:span #:span-content #:span-style #:line #:line-alignment #:text)
  (:import-from #:tui/widgets/widget #:render)
  (:import-from #:tui/widgets/draw #:clip-area #:draw-line #:draw-text))

(in-package #:tui/widgets/text)

;;; A string is drawn on the first row of the area, from its left edge, clipped to its width.
(defmethod render ((string string) area buffer)
  (multiple-value-bind (x y width height) (clip-area area buffer)
    (when (and (plusp width) (plusp height))
      (set-string buffer x y string :max-width width))))

;;; A span is drawn like a string, in its style.
(defmethod render ((span span) area buffer)
  (multiple-value-bind (x y width height) (clip-area area buffer)
    (when (and (plusp width) (plusp height))
      (set-string buffer x y (span-content span) :style (span-style span) :max-width width))))

;;; A line is drawn on the first row of the area, which it patches with its style, aligned
;;; by its alignment and clipped (see DRAW-LINE).
(defmethod render ((line line) area buffer)
  (multiple-value-bind (x y width height) (clip-area area buffer)
    (when (plusp height)
      (draw-line buffer x y width line (line-alignment line)))))

;;; A text patches the area with its style and draws its lines from the top (see DRAW-TEXT).
(defmethod render ((text text) area buffer)
  (multiple-value-bind (x y width height) (clip-area area buffer)
    (draw-text buffer x y width height text)))
