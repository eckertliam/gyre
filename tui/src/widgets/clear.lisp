;;;; clear.lisp — the clear widget: resets an area, typically under a popup, as in ratatui.

(defpackage #:tui/widgets/clear
  (:use #:cl)
  (:import-from #:tui/buffer #:fill-cells #:set-diff-option)
  (:import-from #:tui/widgets/widget #:render)
  (:export #:clear))

(in-package #:tui/widgets/clear)

(defclass clear ()
  ()
  (:documentation "A widget that resets every cell of its area: a space in the terminal's
default colors, without modifiers, drawn by the diff as usual (diff option :none). Render it
before a popup so that what was drawn underneath doesn't show through. Make one with CLEAR."))

(defun clear ()
  "Return a clear widget."
  (make-instance 'clear))

(defmethod render ((widget clear) area buffer)
  (fill-cells buffer area " ")
  (set-diff-option buffer area :none))
