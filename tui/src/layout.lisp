;;;; layout.lisp — splitting a rect into areas by constraints, in the style of ratatui's Layout.
;;;; The public layout API: it also re-exports rects (tui/rect) and the constraint condition.

(defpackage #:tui/layout
  (:use #:cl)
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
                #:rect-clamp)
  (:import-from #:tui/layout/constraint
                #:parse-constraint
                #:invalid-constraint
                #:invalid-constraint-designator)
  (:import-from #:tui/layout/solver
                #:solver
                #:make-solver
                #:solver-count
                #:solver-starts
                #:solver-extents
                #:solve)
  (:export
   ;; Rects, from tui/rect.
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
   #:rect-clamp
   ;; Layouts.
   #:invalid-constraint
   #:invalid-constraint-designator
   #:layout
   #:layoutp
   #:make-layout
   #:layout-direction
   #:layout-constraints
   #:split
   #:split-area))

(in-package #:tui/layout)

(defstruct (layout (:constructor %make-layout
                       (direction constraints horizontal-margin vertical-margin solver))
                   (:predicate layoutp)
                   (:copier nil))
  "A way of dividing a rect into areas: a direction, one constraint per area, a flex mode,
spacing and margins. Make one with MAKE-LAYOUT and divide rects with SPLIT. A layout's
configuration never changes; it also caches the result of its last SPLIT. LAYOUT-DIRECTION
returns its direction and LAYOUT-CONSTRAINTS the list of constraint designators it was made
with."
  (direction :vertical :type (member :vertical :horizontal) :read-only t)
  (constraints '() :type list :read-only t)
  (horizontal-margin 0 :type (and fixnum unsigned-byte) :read-only t)
  (vertical-margin 0 :type (and fixnum unsigned-byte) :read-only t)
  (solver (make-solver #() :start 0) :type solver :read-only t)
  (cached-area nil :type (or null rect))
  (cached-areas #() :type simple-vector))

(defmethod print-object ((layout layout) stream)
  (print-unreadable-object (layout stream :type t :identity t)
    (format stream "~(~A~) ~S" (layout-direction layout) (layout-constraints layout))))

(defun make-layout (&key (direction :vertical) constraints (flex :start) (spacing 0) (margin 0)
                      (horizontal-margin margin) (vertical-margin margin))
  "Return a layout that splits a rect into one area per element of CONSTRAINTS.

DIRECTION is :vertical (areas stacked top to bottom, the default) or :horizontal (left to right).
Each constraint designator says how much space its area wants along that direction:
  (:length n)        n cells,
  (:min n)           at least n cells, and a share of unused space like (:fill 1),
  (:max n)           n cells, and never more,
  (:percentage p)    p percent of the length of the area inside the margins,
  (:ratio n d)       n/d of that length,
  (:fill [weight])   a share of the space the others leave, in proportion to WEIGHT (default 1).
The numbers are non-negative fixnums and D is positive; anything else signals INVALID-CONSTRAINT.
SPLIT describes how conflicts between constraints are resolved.

FLEX says where space that the constraints leave unused goes: :start (after the areas, the
default), :end (before them), :center (half on each side), :space-between (between them),
:space-evenly (equally before, between and after them), :space-around (between them, and half
as much at each end) or :legacy (into the last area of the lowest-priority kind, as ratatui did
before it had flex modes). :legacy, and :space-between with a single area, give unused space to
an area even if it is a length or a max. SPACING is the number of cells between neighbouring
areas: exactly that for :start, :end, :center and :legacy, and at least that for the space-*
modes, where :space-evenly also leaves it at both ends and :space-around leaves it at both ends
and twice it between areas. Negative spacing makes neighbours overlap, except under
:space-evenly and :space-around, which treat it as 0. MARGIN is the number of cells left empty
inside each edge of the rect being split; HORIZONTAL-MARGIN (left and right) and
VERTICAL-MARGIN (top and bottom) override it.

Make a layout once and keep it: SPLIT caches its result in the layout."
  (check-type direction (member :vertical :horizontal))
  (check-type constraints list)
  (check-type margin (and fixnum unsigned-byte))
  (check-type horizontal-margin (and fixnum unsigned-byte))
  (check-type vertical-margin (and fixnum unsigned-byte))
  (%make-layout direction (copy-tree constraints) horizontal-margin vertical-margin
                (make-solver (map 'simple-vector #'parse-constraint constraints) flex spacing)))

(defun split (layout area)
  "Return a simple-vector of rects, one per constraint of LAYOUT and in the same order, that
divide the rect AREA inside LAYOUT's margins along its direction. Each rect spans the whole
inner area across the direction.

The areas are in order and lie inside the inner area. With non-negative spacing they don't
overlap, and when the constraints can absorb all the space (a fill or min is present, or the
sizes add up) they cover the inner area exactly, apart from the spacing. Constraints are
satisfied in priority order: the spacing first; then the minimums of (:min n); then
(:length n); (:percentage p); (:ratio n d); (:max n); and last, the space left is shared by
the fills and the mins (as weight 1, and not under :legacy flex) in proportion to their
weights, a min never dropping below its minimum. When a priority class asks for more than is
left, it takes all of it, shared as evenly as its demands allow (the largest demands are cut
first), and the lower classes get nothing; :legacy flex grants the class in order instead. If
the spacing doesn't fit, the spacers take the whole inner area and every area is empty. If
no fill or min takes the leftover space, the flex mode places it. Sizes are computed in fixed
point and area boundaries are rounded to the nearest cell, halves up, so the areas' sizes add
up exactly to the space they share.

The result is cached in LAYOUT: calling SPLIT again with an area that is RECT= to the last one
returns the same vector without allocating. Callers must not modify the vector. A layout is
not safe to share between threads."
  (let ((cached (layout-cached-area layout)))
    (if (and cached (rect= cached area))
        (layout-cached-areas layout)
        (split-anew layout area))))

(defun split-anew (layout area)
  "Compute LAYOUT's areas for AREA, cache them and return them."
  (let* ((inner (rect-inner area (layout-horizontal-margin layout)
                            (layout-vertical-margin layout)))
         (horizontal (eq (layout-direction layout) :horizontal))
         (solver (solve (layout-solver layout)
                        (if horizontal (rect-width inner) (rect-height inner))))
         (areas (make-array (solver-count solver))))
    (dotimes (i (length areas))
      (let ((start (aref (solver-starts solver) i))
            (extent (aref (solver-extents solver) i)))
        (setf (svref areas i)
              (if horizontal
                  (make-rect (+ (rect-x inner) start) (rect-y inner) extent (rect-height inner))
                  (make-rect (rect-x inner) (+ (rect-y inner) start) (rect-width inner) extent)))))
    (setf (layout-cached-area layout) area
          (layout-cached-areas layout) areas)))

(defun split-area (area direction constraints &rest options
                   &key flex spacing margin horizontal-margin vertical-margin)
  "Split AREA once, as (SPLIT (MAKE-LAYOUT ...) AREA) would with DIRECTION, CONSTRAINTS and the
given options; see MAKE-LAYOUT. It makes a new layout each time, so it allocates: keep a layout
for anything drawn every frame."
  (declare (ignore flex spacing margin horizontal-margin vertical-margin))
  (split (apply #'make-layout :direction direction :constraints constraints options) area))
