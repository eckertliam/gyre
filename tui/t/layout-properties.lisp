;;;; layout-properties.lisp — properties of layout splits, checked on thousands of random cases
;;;; drawn from a fixed seed.

(defpackage #:tui/tests/layout-properties
  (:use #:cl)
  (:import-from #:tui/tests/harness #:deftest #:is)
  (:import-from #:tui/layout
                #:make-rect
                #:rect-x
                #:rect-y
                #:rect-width
                #:rect-height
                #:rect-inner
                #:rect-offset
                #:make-layout
                #:split
                #:split-area))

(in-package #:tui/tests/layout-properties)

;;; Random cases

(defstruct (split-case (:constructor make-split-case
                           (direction constraints flex spacing
                            horizontal-margin vertical-margin area)))
  "The arguments of one random split."
  direction constraints flex spacing horizontal-margin vertical-margin area)

(defconstant +cases+ 4000
  "The number of random cases each property is checked on.")

(defun random-between (low high state)
  (+ low (random (1+ (- high low)) state)))

(defun random-element (vector state)
  (svref vector (random (length vector) state)))

(defun random-constraint (state)
  (ecase (random 6 state)
    (0 (list :length (random 40 state)))
    (1 (list :min (random 40 state)))
    (2 (list :max (random 40 state)))
    (3 (list :percentage (random 120 state)))
    (4 (list :ratio (random 5 state) (random-between 1 5 state)))
    (5 (list :fill (random 4 state)))))

(defun any-constraints (count state)
  (loop repeat count
        collect (random-constraint state)))

(defun random-case (state constraints min-spacing)
  "Return a random split case whose constraints are (FUNCALL CONSTRAINTS count STATE) and
whose spacing is at least MIN-SPACING."
  (make-split-case (random-element #(:horizontal :vertical) state)
                   (funcall constraints (random-between 1 6 state) state)
                   (random-element #(:start :end :center :space-between :space-around
                                     :space-evenly :legacy)
                                   state)
                   (random-between min-spacing 3 state)
                   (random 3 state)
                   (random 3 state)
                   (make-rect (random-between -5 20 state) (random-between -5 20 state)
                              (random 60 state) (random 60 state))))

(defun random-cases (&key (constraints #'any-constraints) (min-spacing 0))
  "Return +CASES+ random cases drawn from a fixed seed; see RANDOM-CASE."
  (let ((state (sb-ext:seed-random-state 42)))
    (loop repeat +cases+
          collect (random-case state constraints min-spacing))))

;;; Looking at a split

(defun case-areas (case)
  (split-area (split-case-area case) (split-case-direction case) (split-case-constraints case)
              :flex (split-case-flex case)
              :spacing (split-case-spacing case)
              :horizontal-margin (split-case-horizontal-margin case)
              :vertical-margin (split-case-vertical-margin case)))

(defun case-inner (case)
  (rect-inner (split-case-area case)
              (split-case-horizontal-margin case)
              (split-case-vertical-margin case)))

(defun horizontalp (case)
  (eq :horizontal (split-case-direction case)))

(defun start (case rect)
  "Return where RECT starts along CASE's direction."
  (if (horizontalp case) (rect-x rect) (rect-y rect)))

(defun extent (case rect)
  "Return RECT's length along CASE's direction."
  (if (horizontalp case) (rect-width rect) (rect-height rect)))

(defun end (case rect)
  (+ (start case rect) (extent case rect)))

(defun line-length (case)
  (extent case (case-inner case)))

(defun count-of (case)
  (length (split-case-constraints case)))

(defun reserved-spacing (case)
  "Return the cells CASE's flex mode reserves for spacing, as MAKE-LAYOUT documents it."
  (let ((spacing (split-case-spacing case))
        (count (count-of case)))
    (ecase (split-case-flex case)
      ((:start :end :center :legacy :space-between) (* spacing (1- count)))
      (:space-evenly (* (max 0 spacing) (1+ count)))
      (:space-around (* (max 0 spacing) 2 count)))))

(defun available (case)
  "Return the cells left for the areas once the spacing is reserved."
  (- (line-length case) (reserved-spacing case)))

(defun spacing-fits-p (case)
  (<= 0 (available case)))

(defun spacers (case)
  "Return the spacers of CASE's split, in cells: before the first area, between neighbours, and
after the last. Overlapping neighbours give a negative spacer."
  (let* ((areas (coerce (case-areas case) 'list))
         (inner (case-inner case))
         (ends (cons (start case inner) (mapcar (lambda (area) (end case area)) areas)))
         (starts (append (mapcar (lambda (area) (start case area)) areas)
                         (list (end case inner)))))
    (mapcar #'- starts ends)))

(defun minimum-spacers (case)
  "Return the smallest spacers CASE's flex mode allows with non-negative spacing, as SPACERS
lists them."
  (let ((spacing (split-case-spacing case))
        (count (count-of case)))
    (multiple-value-bind (edge middle)
        (ecase (split-case-flex case)
          ((:start :end :center :legacy :space-between) (values 0 spacing))
          (:space-evenly (values spacing spacing))
          (:space-around (values spacing (* 2 spacing))))
      (append (list edge) (make-list (1- count) :initial-element middle) (list edge)))))

(defun kinds (case)
  (mapcar #'first (split-case-constraints case)))

(defun values-of (kind case)
  "Return the value of each constraint of KIND in CASE, with the area it got."
  (loop for (constraint-kind value) in (split-case-constraints case)
        for area across (case-areas case)
        when (eq kind constraint-kind)
          collect (list value (extent case area))))

(defun satisfiablep (case)
  "Return true if CASE's constraints absorb the whole line, leaving every spacer at its minimum.
That is when the spacing is non-negative and fits, and either the flex mode gives unused space
to an area (:legacy, or :space-between with a single area), or a constraint grows into unused
space (a fill, or a min outside :legacy), or the constraints are all lengths adding up exactly
to the line minus the spacing."
  (let ((kinds (kinds case))
        (flex (split-case-flex case)))
    (and (<= 0 (split-case-spacing case))
         (spacing-fits-p case)
         (or (eq flex :legacy)
             (and (eq flex :space-between) (= 1 (count-of case)))
             (member :fill kinds)
             (member :min kinds)
             (and (every (lambda (kind) (eq kind :length)) kinds)
                  (= (available case)
                     (reduce #'+ (split-case-constraints case) :key #'second)))))))

(defun counterexamples (property cases)
  "Return up to three (case reason) lists for which PROPERTY returns a reason rather than NIL."
  (let ((failures '()))
    (dolist (case cases (nreverse failures))
      (let ((reason (funcall property case)))
        (when reason
          (push (list case reason) failures)
          (when (= 3 (length failures))
            (return (nreverse failures))))))))

;;; Areas are inside the inner area and in order

(defun inside-and-in-order (case)
  "Return a reason unless CASE's split gives one area per constraint, each spanning the inner
area across the direction and lying inside it along the direction, with each area starting no
earlier than the end of the previous one plus the spacing (or 0 when the spacing doesn't fit):
so in order and without overlaps when the spacing is non-negative, and overlapping by at most
the spacing when it is negative (which :space-evenly and :space-around treat as 0)."
  (let* ((areas (coerce (case-areas case) 'list))
         (inner (case-inner case))
         (spacing (split-case-spacing case))
         (gap (cond ((not (spacing-fits-p case)) 0)
                    ((member (split-case-flex case) '(:space-evenly :space-around))
                     (max 0 spacing))
                    (t spacing))))
    (flet ((across (rect)
             (if (horizontalp case)
                 (list (rect-y rect) (rect-height rect))
                 (list (rect-x rect) (rect-width rect)))))
      (cond ((/= (length areas) (count-of case)) :area-count)
            ((notevery (lambda (area) (equal (across area) (across inner))) areas) :across)
            ((notevery (lambda (area)
                         (<= (start case inner) (start case area) (end case area)
                             (end case inner)))
                       areas)
             :outside)
            ((notevery (lambda (a b) (<= (+ (end case a) gap) (start case b)))
                       areas (rest areas))
             :order)))))

(deftest split-areas-are-inside-and-in-order
  (is (equal '() (counterexamples #'inside-and-in-order (random-cases :min-spacing -2)))))

;;; Spacing

(defun spread (numbers)
  "Return the difference between the largest and the smallest of NUMBERS, or 0 if none."
  (if numbers (- (reduce #'max numbers) (reduce #'min numbers)) 0))

(defun spacing-respected (case)
  "Return a reason unless every spacer of CASE's split is at least its minimum, the spacing is
exact for :start, :end, :center and :legacy, and the unused space is where the flex mode puts
it (spacers meant to be equal differ by at most a cell after rounding); or, when the spacing
doesn't fit, unless every area is empty."
  (let* ((spacers (spacers case))
         (edges (list (first spacers) (car (last spacers))))
         (middles (butlast (rest spacers)))
         (flex (split-case-flex case)))
    (cond ((not (spacing-fits-p case))
           (unless (every (lambda (area) (zerop (extent case area))) (case-areas case))
             :areas-despite-spacing))
          ((notevery #'<= (minimum-spacers case) spacers) :below-minimum)
          ((and (member flex '(:start :end :center :legacy))
                (notevery (lambda (middle) (= middle (split-case-spacing case))) middles))
           :inexact-spacing)
          ((and (member flex '(:start :legacy :space-between)) (/= 0 (first edges)))
           :leading-space)
          ((and (member flex '(:end :legacy :space-between)) (/= 0 (second edges)))
           :trailing-space)
          ((and (member flex '(:center :space-around)) (< 1 (spread edges))) :uneven-edges)
          ((and (member flex '(:space-between :space-around)) (< 1 (spread middles)))
           :uneven-middles)
          ((and (eq flex :space-evenly) (< 1 (spread spacers))) :uneven-spacers))))

(deftest split-respects-spacing
  (is (equal '() (counterexamples #'spacing-respected (random-cases)))))

;;; Coverage

(defun exact-lengths (count state)
  "Return COUNT random lengths. COVERAGE-CASES rescales them to fill a line exactly."
  (loop repeat count
        collect (list :length (random 30 state))))

(defun rescale-lengths (case)
  "Return CASE with its lengths changed to add up to its available space, if they can."
  (let* ((available (available case))
         (constraints (split-case-constraints case))
         (total (reduce #'+ constraints :key #'second)))
    (when (and (plusp total) (<= 0 available))
      (let ((lengths (mapcar (lambda (c) (floor (* available (second c)) total)) constraints)))
        (incf (first lengths) (- available (reduce #'+ lengths)))
        (setf (split-case-constraints case)
              (mapcar (lambda (length) (list :length length)) lengths))))
    case))

(defun coverage-cases ()
  "Return random cases, half of them made only of lengths that add up to the available space."
  (append (random-cases)
          (mapcar #'rescale-lengths (random-cases :constraints #'exact-lengths))))

(defun covered-exactly (case)
  "Return a reason if CASE is satisfiable but its spacers are not all at their minimum, which
would mean that the areas don't cover the line exactly apart from the spacing."
  (when (and (satisfiablep case)
             (not (equal (spacers case) (minimum-spacers case))))
    :not-covered))

(deftest split-covers-the-line-when-satisfiable
  (let ((cases (coverage-cases)))
    (is (equal '() (counterexamples #'covered-exactly cases)))
    ;; Enough of the cases are satisfiable for the property to mean something.
    (is (< (floor (length cases) 3) (count-if #'satisfiablep cases)))))

;;; Constraints that fit are honoured

(defun constraints-honoured (case)
  "Return a reason if, with non-negative spacing that fits, an area breaks its constraint: a
min smaller than its minimum when all minimums fit; and outside :legacy and a lone area under
:space-between (which absorb unused space), a length or max larger than its value, lengths
that all fit but aren't exact, or percentages that all fit but are off by a cell or more."
  (let ((free (available case))
        (kinds (kinds case))
        (flex (split-case-flex case)))
    (flet ((total (kind) (reduce #'+ (values-of kind case) :key #'first)))
      (cond ((or (minusp (split-case-spacing case)) (minusp free)) nil)
            ((and (<= (total :min) free)
                  (notevery (lambda (pair) (apply #'<= pair)) (values-of :min case)))
             :min-too-small)
            ((or (eq flex :legacy) (and (eq flex :space-between) (= 1 (count-of case)))) nil)
            ((notevery (lambda (pair) (apply #'>= pair))
                       (append (values-of :length case) (values-of :max case)))
             :too-large)
            ((and (every (lambda (kind) (eq kind :length)) kinds) (<= (total :length) free)
                  (notevery (lambda (pair) (apply #'= pair)) (values-of :length case)))
             :length-not-exact)
            ((and (every (lambda (kind) (eq kind :percentage)) kinds)
                  (<= (* (line-length case) (total :percentage)) (* 100 free))
                  (notevery (lambda (pair)
                              (< (abs (- (* 100 (second pair)) (* (first pair) (line-length case))))
                                 100))
                            (values-of :percentage case)))
             :percentage-off)))))

(defun percentages (count state)
  (loop repeat count
        collect (list :percentage (random 40 state))))

(deftest split-honours-constraints-that-fit
  (is (equal '() (counterexamples #'constraints-honoured (random-cases))))
  (is (equal '() (counterexamples #'constraints-honoured
                                  (random-cases :constraints #'percentages)))))

;;; Fills share by weight

(defun fills (count state)
  (loop repeat count
        collect (list :fill (random-between 1 5 state))))

(defun shared-by-weight (case)
  "Return a reason if CASE's fills, when the spacing fits, are not each within a cell of their
share of the available space by weight."
  (let ((free (available case))
        (weights (reduce #'+ (split-case-constraints case) :key #'second)))
    (when (and (<= 0 (split-case-spacing case)) (<= 0 free)
               (notevery (lambda (pair)
                           (destructuring-bind (weight extent) pair
                             (< (abs (- (* extent weights) (* free weight))) weights)))
                         (values-of :fill case)))
      :not-by-weight)))

(deftest split-shares-fills-by-weight
  (is (equal '() (counterexamples #'shared-by-weight (random-cases :constraints #'fills)))))

;;; Splits don't depend on where the area is, or on the direction

(defun moved (case dx dy)
  (let ((moved (copy-split-case case)))
    (setf (split-case-area moved) (rect-offset (split-case-area case) dx dy))
    moved))

(defun transposed (case)
  "Return CASE in the other direction, with its area and margins transposed."
  (let ((area (split-case-area case)))
    (make-split-case (if (horizontalp case) :vertical :horizontal)
                     (split-case-constraints case)
                     (split-case-flex case)
                     (split-case-spacing case)
                     (split-case-vertical-margin case)
                     (split-case-horizontal-margin case)
                     (make-rect (rect-y area) (rect-x area) (rect-height area) (rect-width area)))))

(defun transpose-rect (rect)
  (make-rect (rect-y rect) (rect-x rect) (rect-height rect) (rect-width rect)))

(defun not-invariant (case)
  "Return a reason if moving CASE's area moves its areas differently, or if transposing CASE
doesn't transpose its areas."
  (cond ((not (equalp (map 'vector (lambda (area) (rect-offset area 7 -3)) (case-areas case))
                      (case-areas (moved case 7 -3))))
         :moved)
        ((not (equalp (map 'vector #'transpose-rect (case-areas case))
                      (case-areas (transposed case))))
         :transposed)))

(deftest split-is-invariant-under-moves-and-transposition
  (is (equal '() (counterexamples #'not-invariant (random-cases :min-spacing -2)))))

;;; A layout gives the same areas as SPLIT-AREA, and caches them

(defun layout-differs (case)
  (let* ((layout (make-layout :direction (split-case-direction case)
                              :constraints (split-case-constraints case)
                              :flex (split-case-flex case)
                              :spacing (split-case-spacing case)
                              :horizontal-margin (split-case-horizontal-margin case)
                              :vertical-margin (split-case-vertical-margin case)))
         (areas (split layout (split-case-area case))))
    (cond ((not (equalp areas (case-areas case))) :differs)
          ((not (eq areas (split layout (copy-structure (split-case-area case))))) :not-cached))))

(deftest layout-split-matches-split-area
  (is (equal '() (counterexamples #'layout-differs (random-cases :min-spacing -2)))))
