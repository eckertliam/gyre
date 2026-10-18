;;;; mouse.lisp — decoding mouse reports: SGR (1006), urxvt (1015) and X10 normal tracking.

(defpackage #:tui/input/mouse
  (:use #:cl)
  (:import-from #:tui/input/event #:+shift+ #:+alt+ #:+ctrl+ #:make-mouse-event)
  (:import-from #:tui/input/params #:param)
  (:export #:sgr-mouse-event #:urxvt-mouse-event #:x10-mouse-event))

(in-package #:tui/input/mouse)

(defun button-modifiers (code)
  (declare (type fixnum code))
  (logior (if (logtest code 4) +shift+ 0)
          (if (logtest code 8) +alt+ 0)
          (if (logtest code 16) +ctrl+ 0)))

(defun button-keyword (number)
  (declare (type fixnum number))
  (case number
    (0 :left) (1 :middle) (2 :right)
    (8 :button-8) (9 :button-9) (10 :button-10) (11 :button-11)))

(defun mouse-kind (number button motion release)
  (declare (type fixnum number))
  (cond (motion (if button :drag :motion))
        ((<= 4 number 7)
         (svref #(:scroll-up :scroll-down :scroll-left :scroll-right) (- number 4)))
        ((or release (= number 3)) :release)
        (t :press)))

(defun mouse-event-from-code (code x y release)
  "Return the mouse event for button code CODE (the protocol's Cb without any
offset) at 0-based X and Y, or nil when CODE names no known button. RELEASE is
true for an SGR release report."
  (declare (type fixnum code x y))
  ;; Bits 0-1 are the button, +64 selects buttons 4-7 (the wheel), +128 buttons 8-11.
  (let ((number (+ (logand code 3)
                   (if (logtest code 64) 4 0)
                   (if (logtest code 128) 8 0))))
    (when (and (< code 256) (< number 12))
      (let* ((button (button-keyword number))
             (kind (mouse-kind number button (logtest code 32) release)))
        (make-mouse-event :kind kind
                          :button (and (member kind '(:press :release :drag)) button)
                          :x (max 0 x)
                          :y (max 0 y)
                          :modifiers (button-modifiers code))))))

(defun sgr-mouse-event (params release)
  "Return the event for the SGR report CSI < PARAMS M (or m when RELEASE), or nil."
  (let ((code (param params 0))
        (x (param params 1))
        (y (param params 2)))
    (when (and (>= code 0) (>= x 0) (>= y 0))
      (mouse-event-from-code code (1- x) (1- y) release))))

(defun urxvt-mouse-event (params)
  "Return the event for the urxvt report CSI Cb ; Cx ; Cy M, or nil."
  (let ((code (param params 0))
        (x (param params 1))
        (y (param params 2)))
    (when (and (>= code 32) (>= x 0) (>= y 0))
      (mouse-event-from-code (- code 32) (1- x) (1- y) nil))))

(defun x10-mouse-event (cb cx cy)
  "Return the event for the X10 report CSI M followed by the bytes CB, CX and CY,
or nil."
  (declare (type (unsigned-byte 8) cb cx cy))
  (when (>= cb 32)
    (mouse-event-from-code (- cb 32) (- cx 33) (- cy 33) nil)))
