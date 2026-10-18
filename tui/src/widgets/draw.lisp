;;;; draw.lisp — helpers shared by the built-in widgets. Not part of the public API. The
;;;; drawing itself (regions, lines, texts) is in tui/buffer and tui/buffer/text.

(defpackage #:tui/widgets/draw
  (:use #:cl)
  (:import-from #:tui/layout #:rect-x #:rect-y #:rect-width #:rect-height)
  (:import-from #:tui/buffer #:clip-region)
  (:import-from #:tui/text #:line #:make-line #:text #:make-text)
  (:export #:clip-area
           #:as-text
           #:as-line
           #:highlight-spacing
           #:selection-column-p))

(in-package #:tui/widgets/draw)

;;; Regions

(defun clip-area (area buffer)
  "Return the part of the rect AREA that lies inside BUFFER as four values X, Y, WIDTH and
HEIGHT, as CLIP-REGION does."
  (clip-region buffer (rect-x area) (rect-y area) (rect-width area) (rect-height area)))

;;; Lines and texts

(defun as-text (designator)
  "Return the text DESIGNATOR designates (see MAKE-TEXT); a text is returned as it is, keeping
its style and alignment for the whole text rather than folding them into its lines."
  (if (typep designator 'text) designator (make-text designator)))

(defun as-line (designator)
  "Return the line DESIGNATOR designates (see MAKE-LINE); a line is returned as it is."
  (if (typep designator 'line) designator (make-line designator)))

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
