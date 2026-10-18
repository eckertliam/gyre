;;;; widget.lisp — the widget protocol: generic functions that draw a widget into an area of a
;;;; buffer, with or without a state that persists between frames.

(defpackage #:tui/widgets/widget
  (:use #:cl)
  (:import-from #:tui/render #:frame-buffer #:frame-area)
  (:export #:render
           #:render-stateful
           #:render-widget
           #:render-stateful-widget))

(in-package #:tui/widgets/widget)

(defgeneric render (widget area buffer)
  (:documentation "Draw WIDGET into the rect AREA of BUFFER.

Methods draw only inside AREA (and inside BUFFER: the parts of AREA outside it are clipped),
patch the styles of the cells they draw over rather than replacing them unless documented
otherwise, and don't keep AREA or BUFFER after returning. Rendering a widget doesn't modify
it, so a widget can be built once and rendered every frame; methods for widgets meant to be
drawn every frame shouldn't allocate.

Methods are provided for strings, spans, lines and texts (see TUI/TEXT), for NIL (draws
nothing) and for the built-in widgets. Applications and plugins add methods for their own
widget classes."))

(defgeneric render-stateful (widget area buffer state)
  (:documentation "Draw WIDGET into the rect AREA of BUFFER, as RENDER does, using and updating
STATE: an object that outlives the frame and holds what the application and the widget both
change, such as a selection and a scroll offset. Methods may modify STATE, for example to
keep a selected item visible, and must not keep AREA, BUFFER or STATE after returning.

Methods are provided for the built-in stateful widgets: a LIST with a LIST-STATE, a TABLE
with a TABLE-STATE and a SCROLLBAR with a SCROLLBAR-STATE."))

(defmethod render ((widget null) area buffer)
  (declare (ignore area buffer))
  nil)

(defun render-widget (frame widget &optional area)
  "Render WIDGET into FRAME's buffer, in AREA (by default the whole frame), with RENDER."
  (render widget (or area (frame-area frame)) (frame-buffer frame)))

(defun render-stateful-widget (frame widget state &optional area)
  "Render WIDGET with STATE into FRAME's buffer, in AREA (by default the whole frame), with
RENDER-STATEFUL."
  (render-stateful widget (or area (frame-area frame)) (frame-buffer frame) state))
