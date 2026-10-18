;;;; layout.lisp — TEMPORARY stub so buffer/render can compile until tui/layout is merged.

(defpackage #:tui/layout
  (:use #:cl)
  (:export #:rect #:make-rect #:rect-x #:rect-y #:rect-width #:rect-height
           #:rect-right #:rect-bottom #:rect-intersection #:rect-empty-p #:rect-area))

(in-package #:tui/layout)

(defstruct (rect (:constructor make-rect (x y width height)))
  (x 0 :type fixnum :read-only t)
  (y 0 :type fixnum :read-only t)
  (width 0 :type fixnum :read-only t)
  (height 0 :type fixnum :read-only t))

(defun rect-right (r) (+ (rect-x r) (rect-width r)))
(defun rect-bottom (r) (+ (rect-y r) (rect-height r)))
(defun rect-area (r) (* (rect-width r) (rect-height r)))
(defun rect-empty-p (r) (or (zerop (rect-width r)) (zerop (rect-height r))))
(defun rect-intersection (a b)
  (let ((x (max (rect-x a) (rect-x b)))
        (y (max (rect-y a) (rect-y b))))
    (make-rect x y
               (max 0 (- (min (rect-right a) (rect-right b)) x))
               (max 0 (- (min (rect-bottom a) (rect-bottom b)) y)))))
