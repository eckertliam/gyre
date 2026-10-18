;;;; rect.lisp — rectangles of terminal cells, the value type that layouts produce and widgets
;;;; draw into.

(defpackage #:tui/rect
  (:use #:cl)
  (:export #:rect
           #:rect-p
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

(in-package #:tui/rect)

(defstruct (rect (:constructor %make-rect (x y width height))
                 (:copier nil))
  "A rectangle of terminal cells: columns X to X+WIDTH-1 of rows Y to Y+HEIGHT-1. X and Y are
fixnums and may be negative; WIDTH and HEIGHT are non-negative fixnums. A rect is an immutable
value: its slots are read-only, operations return new rects, and RECT= compares two rects."
  (x 0 :type fixnum :read-only t)
  (y 0 :type fixnum :read-only t)
  (width 0 :type (and fixnum unsigned-byte) :read-only t)
  (height 0 :type (and fixnum unsigned-byte) :read-only t))

(declaim (inline make-rect rect-right rect-bottom rect-area rect-empty-p rect=))

(defun make-rect (x y width height)
  "Return the rect whose top-left cell is column X of row Y, WIDTH columns wide and HEIGHT rows
tall. A negative WIDTH or HEIGHT is taken as 0."
  (declare (fixnum x y width height))
  (%make-rect x y (max 0 width) (max 0 height)))

(defun rect-right (rect)
  "Return the column just past RECT's right edge: X + WIDTH."
  (+ (rect-x rect) (rect-width rect)))

(defun rect-bottom (rect)
  "Return the row just past RECT's bottom edge: Y + HEIGHT."
  (+ (rect-y rect) (rect-height rect)))

(defun rect-area (rect)
  "Return the number of cells in RECT."
  (* (rect-width rect) (rect-height rect)))

(defun rect-empty-p (rect)
  "Return true if RECT contains no cells, that is if its width or height is 0."
  (or (zerop (rect-width rect)) (zerop (rect-height rect))))

(defun rect= (a b)
  "Return true if rects A and B have the same position and size."
  (and (= (rect-x a) (rect-x b))
       (= (rect-y a) (rect-y b))
       (= (rect-width a) (rect-width b))
       (= (rect-height a) (rect-height b))))

(defun rect-intersection (a b)
  "Return the rect of the cells that are in both A and B. When A and B don't overlap, the result
is empty and positioned at the larger of their X and the larger of their Y."
  (let ((x (max (rect-x a) (rect-x b)))
        (y (max (rect-y a) (rect-y b))))
    (make-rect x y
               (- (min (rect-right a) (rect-right b)) x)
               (- (min (rect-bottom a) (rect-bottom b)) y))))

(defun rect-union (a b)
  "Return the smallest rect that contains both A and B. As in ratatui, an empty rect still counts
by its position."
  (let ((x (min (rect-x a) (rect-x b)))
        (y (min (rect-y a) (rect-y b))))
    (make-rect x y
               (- (max (rect-right a) (rect-right b)) x)
               (- (max (rect-bottom a) (rect-bottom b)) y))))

(defun rect-intersects-p (a b)
  "Return true if A and B have at least one cell in common. Empty rects intersect nothing."
  (and (not (rect-empty-p a))
       (not (rect-empty-p b))
       (< (rect-x a) (rect-right b))
       (< (rect-x b) (rect-right a))
       (< (rect-y a) (rect-bottom b))
       (< (rect-y b) (rect-bottom a))))

(defun rect-contains-p (rect x y)
  "Return true if the cell at column X of row Y is inside RECT."
  (and (<= (rect-x rect) x)
       (< x (rect-right rect))
       (<= (rect-y rect) y)
       (< y (rect-bottom rect))))

(defun rect-inner (rect horizontal &optional (vertical horizontal))
  "Return RECT shrunk by HORIZONTAL columns on the left and on the right, and by VERTICAL rows
(by default HORIZONTAL) at the top and at the bottom. Both margins are non-negative fixnums. When
a margin doesn't fit, that dimension becomes 0 and the result still lies within RECT."
  (check-type horizontal (and fixnum unsigned-byte))
  (check-type vertical (and fixnum unsigned-byte))
  (let ((width (rect-width rect))
        (height (rect-height rect)))
    (make-rect (+ (rect-x rect) (min horizontal width))
               (+ (rect-y rect) (min vertical height))
               (max 0 (- width (* 2 horizontal)))
               (max 0 (- height (* 2 vertical))))))

(defun rect-offset (rect dx dy)
  "Return RECT moved DX columns right and DY rows down; negative amounts move it left or up."
  (declare (fixnum dx dy))
  (make-rect (+ (rect-x rect) dx) (+ (rect-y rect) dy) (rect-width rect) (rect-height rect)))

(defun rect-clamp (rect bounds)
  "Return RECT moved, and shrunk where it is larger than BOUNDS, so that it lies inside BOUNDS."
  (let ((width (min (rect-width rect) (rect-width bounds)))
        (height (min (rect-height rect) (rect-height bounds))))
    (make-rect (max (rect-x bounds) (min (rect-x rect) (- (rect-right bounds) width)))
               (max (rect-y bounds) (min (rect-y rect) (- (rect-bottom bounds) height)))
               width
               height)))
