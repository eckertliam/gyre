;;;; rect.lisp — tests for tui/rect, mostly ported from ratatui's Rect tests.

(defpackage #:tui/tests/rect
  (:use #:cl)
  (:import-from #:tui/tests/harness #:deftest #:is #:signals)
  (:import-from #:tui/rect
                #:rect
                #:rectp
                #:make-rect
                #:rect-x
                #:rect-y
                #:rect-width
                #:rect-height
                #:rect-right
                #:rect-bottom
                #:rect-area
                #:rect-empty-p
                #:rect=
                #:rect-intersection
                #:rect-union
                #:rect-intersects-p
                #:rect-contains-p
                #:rect-inner
                #:rect-offset
                #:rect-clamp))

(in-package #:tui/tests/rect)

(defun r (x y width height)
  (make-rect x y width height))

(deftest rect-accessors
  (let ((rect (r 1 2 3 4)))
    (is (rectp rect))
    (is (typep rect 'rect))
    (is (equal '(1 2 3 4 4 6 12)
               (list (rect-x rect) (rect-y rect) (rect-width rect) (rect-height rect)
                     (rect-right rect) (rect-bottom rect) (rect-area rect))))
    (is (equal '(-3 -4 -1 -2) (list (rect-x (r -3 -4 2 2)) (rect-y (r -3 -4 2 2))
                                    (rect-right (r -3 -4 2 2)) (rect-bottom (r -3 -4 2 2)))))))

(deftest rect-negative-sizes-are-zero
  (is (rect= (r 5 6 0 0) (r 5 6 -3 -1)))
  (is (= 0 (rect-area (r 5 6 -3 4)))))

(deftest rect-equality
  (is (rect= (r 1 2 3 4) (r 1 2 3 4)))
  (dolist (other (list (r 0 2 3 4) (r 1 0 3 4) (r 1 2 0 4) (r 1 2 3 0)))
    (is (not (rect= (r 1 2 3 4) other)))))

(deftest rect-emptiness
  (is (not (rect-empty-p (r 1 2 3 4))))
  (is (rect-empty-p (r 1 2 0 4)))
  (is (rect-empty-p (r 1 2 3 0)))
  (is (rect-empty-p (r 0 0 0 0))))

(deftest rect-intersections
  (is (rect= (r 2 3 2 3) (rect-intersection (r 1 2 3 4) (r 2 3 4 5))))
  (is (rect= (r 4 4 0 0) (rect-intersection (r 1 1 2 2) (r 4 4 2 2))))
  (is (rect= (r 5 5 10 10) (rect-intersection (r 0 0 20 20) (r 5 5 10 10))))
  (is (rect= (r 0 0 1 1) (rect-intersection (r -5 -5 6 6) (r 0 0 3 3))))
  ;; Corner, edge, disjoint and contained pairs intersect the same both ways.
  (loop for (a b) in (list (list (r 0 0 10 10) (r 10 10 20 20))
                           (list (r 0 0 10 10) (r 10 0 20 10))
                           (list (r 0 0 10 10) (r 11 11 20 20))
                           (list (r 0 0 20 20) (r 5 5 10 10)))
        do (is (rect= (rect-intersection a b) (rect-intersection b a)))
           (is (eq (rect-intersects-p a b) (rect-intersects-p b a)))))

(deftest rect-intersects
  (is (rect-intersects-p (r 1 2 3 4) (r 2 3 4 5)))
  (is (not (rect-intersects-p (r 1 2 3 4) (r 5 6 7 8))))
  (is (not (rect-intersects-p (r 0 0 10 10) (r 10 0 5 5))))
  ;; Empty rects intersect nothing, even inside another rect.
  (loop for (a b) in (list (list (r 0 0 10 10) (r 5 5 0 0))
                           (list (r 0 0 1 2) (r 0 1 1 0))
                           (list (r 0 0 2 1) (r 1 0 0 1))
                           (list (r 3 3 0 0) (r 3 3 0 0)))
        do (is (not (rect-intersects-p a b)))
           (is (not (rect-intersects-p b a)))
           (is (rect-empty-p (rect-intersection a b))))
  (loop for (a b) in (list (list (r 0 0 10 10) (r 5 5 1 1))
                           (list (r 1 2 1 1) (r 1 2 1 1)))
        do (is (rect-intersects-p a b))
           (is (rect-intersects-p b a))
           (is (not (rect-empty-p (rect-intersection a b))))))

(deftest rect-unions
  (is (rect= (r 1 2 5 6) (rect-union (r 1 2 3 4) (r 2 3 4 5))))
  (is (rect= (r 0 0 20 20) (rect-union (r 0 0 20 20) (r 5 5 10 10))))
  (is (rect= (r -2 -1 7 6) (rect-union (r -2 -1 1 1) (r 3 3 2 2))))
  ;; As in ratatui, an empty rect still counts by its position.
  (is (rect= (r 0 0 7 7) (rect-union (r 0 0 0 0) (r 5 5 2 2)))))

(deftest rect-contains
  ;; The rect's cells are columns 1 to 3 of rows 2 to 5.
  (let ((rect (r 1 2 3 4)))
    (loop for (x y inside) in '((1 2 t) (3 2 t) (1 5 t) (3 5 t)
                                (0 2 nil) (4 2 nil) (1 1 nil) (1 6 nil) (0 1 nil) (4 6 nil))
          do (is (eq inside (rect-contains-p rect x y)))))
  (is (not (rect-contains-p (r 5 5 0 0) 5 5)))
  (is (rect-contains-p (r -3 -3 2 2) -2 -3)))

(deftest rect-inner-margins
  (is (rect= (r 2 4 1 0) (rect-inner (r 1 2 3 4) 1 2)))
  (is (rect= (r 2 3 6 6) (rect-inner (r 1 2 8 8) 1)))
  (is (rect= (r 1 2 8 8) (rect-inner (r 1 2 8 8) 0 0)))
  (is (rect= (r 3 2 4 8) (rect-inner (r 1 2 8 8) 2 0)))
  ;; A margin that doesn't fit empties that dimension, inside the rect.
  (is (rect= (r 2 4 0 2) (rect-inner (r 1 3 1 4) 3 1)))
  (is (rect= (r 6 6 0 0) (rect-inner (r 0 0 6 6) 10)))
  (is (rect= (r 2 3 0 0) (rect-inner (r 2 3 0 0) 1)))
  (signals type-error (rect-inner (r 0 0 5 5) -1)))

(deftest rect-offsets
  (is (rect= (r 6 8 3 4) (rect-offset (r 1 2 3 4) 5 6)))
  (is (rect= (r 2 2 3 4) (rect-offset (r 4 3 3 4) -2 -1)))
  ;; Unlike ratatui, which saturates at 0, positions may become negative.
  (is (rect= (r -4 -4 3 4) (rect-offset (r 1 2 3 4) -5 -6))))

(deftest rect-clamping
  ;; ratatui: rect::tests::clamp, inside a 100 x 100 rect at (10, 10).
  (let ((bounds (r 10 10 100 100)))
    (loop for (rect expected)
            in (list (list (r 20 20 10 10) (r 20 20 10 10))
                     (list (r 5 5 10 10) (r 10 10 10 10))
                     (list (r 20 5 10 10) (r 20 10 10 10))
                     (list (r 105 5 10 10) (r 100 10 10 10))
                     (list (r 5 20 10 10) (r 10 20 10 10))
                     (list (r 105 20 10 10) (r 100 20 10 10))
                     (list (r 5 105 10 10) (r 10 100 10 10))
                     (list (r 20 105 10 10) (r 20 100 10 10))
                     (list (r 105 105 10 10) (r 100 100 10 10))
                     (list (r 5 20 200 10) (r 10 20 100 10))
                     (list (r 20 5 10 200) (r 20 10 10 100))
                     (list (r 0 0 200 200) (r 10 10 100 100)))
          do (is (rect= expected (rect-clamp rect bounds)))))
  (is (rect= (r 3 4 0 0) (rect-clamp (r 9 9 5 5) (r 3 4 0 0)))))
