;;;; solver.lisp — the one-dimensional layout algorithm: the positions and sizes of the segments
;;;; of a line, from their constraints, a flex mode and the spacing between them.

(defpackage #:tui/layout/solver
  (:use #:cl)
  (:import-from #:tui/layout/constraint
                #:constraint-kind
                #:constraint-value
                #:constraint-denominator)
  (:export #:flex
           #:solver
           #:make-solver
           #:solver-count
           #:solver-flex
           #:solver-spacing
           #:solver-starts
           #:solver-extents
           #:solve))

(in-package #:tui/layout/solver)

;;; How a line is divided
;;;
;;; SOLVE divides a line of LENGTH cells (a layout's area along its direction, inside its
;;; margins) into one segment per constraint, with spacers before, between and after them.
;;; Ratatui does this with the Cassowary solver and weighted constraints. This is a direct
;;; algorithm that settles conflicts in the same order of priority, and gives the same results
;;; for every case in ratatui's layout test table (ported to tui/t/layout*.lisp). It works in
;;; fixed-point integers (1/+SCALE+ of a cell) and rounds only at the end.
;;;
;;; 1. Spacing comes first. SPACING cells are reserved between neighbouring segments for :start,
;;;    :end, :center, :legacy and :space-between. :space-evenly reserves SPACING for every spacer
;;;    including the two at the edges, and :space-around reserves SPACING at each edge and twice
;;;    that between segments. If the reservation doesn't fit in the line, the spacers share the
;;;    whole line in those proportions and every segment is empty. A negative SPACING makes
;;;    neighbours overlap; :space-evenly and :space-around treat it as 0.
;;;
;;; 2. Sizes. The rest of the line (AVAILABLE) is granted to the constraints in this order of
;;;    priority, each demand capped at AVAILABLE:
;;;      a. (:min n) gets n,
;;;      b. (:length n) gets n,
;;;      c. (:percentage p) gets p% of LENGTH,
;;;      d. (:ratio a b) gets a/b of LENGTH,
;;;      e. (:max n) gets n, which is also its upper bound.
;;;    When one of these classes asks for more than is left, it gets everything that is left and
;;;    the later classes get nothing. Within the class the space is shared as evenly as the
;;;    demands allow: a level rises until the space is used up and each constraint gets the
;;;    smaller of its demand and the level, so the largest demands are cut first. :legacy
;;;    instead grants the class in order until the space runs out.
;;;
;;; 3. Growth. Whatever step 2 leaves goes to the fills in proportion to their weights. Except
;;;    under :legacy, a min grows too, like a fill of weight 1 whose size never drops below what
;;;    it got in step 2. Fills of weight 0 get nothing unless every growing constraint has weight
;;;    0, in which case they share equally.
;;;
;;; 4. Excess. If nothing grows, the space left over goes to the spacers: after the segments for
;;;    :start, before them for :end, half before and half after for :center, shared between them
;;;    for :space-between, and added to every spacer in proportion to its share of the spacing
;;;    for :space-evenly and :space-around. Two modes give it to a segment instead:
;;;    :space-between when there is only one segment, and :legacy, which gives it to the last
;;;    segment of the first kind present in the order min, ratio, percentage, length, max.
;;;
;;; 5. Rounding. A segment's start and end are running sums of the spacer and segment sizes
;;;    before it, each rounded to the nearest cell boundary with halves rounding up. Rounding
;;;    positions rather than sizes keeps the segments in order and makes their sizes add up
;;;    exactly to the space they were given.

(deftype flex ()
  "How a layout places its segments when their constraints leave part of the line unused."
  '(member :start :end :center :space-between :space-around :space-evenly :legacy))

(defconstant +scale+ 3603600
  "Fixed-point units per cell. It is divisible by every integer from 1 to 16 and by 100, so
halves, thirds and so on, and whole percentages, of a whole number of cells are exact, and a
boundary that falls exactly halfway between two cells is seen as such when it is rounded.")

(deftype fixnum-vector ()
  '(simple-array fixnum (*)))

(defstruct (solver (:constructor %make-solver
                       (constraints flex spacing demands sizes pinned starts extents))
                   (:copier nil))
  "The constraints, flex mode and spacing of a line, with the working space of SOLVE and its
results. DEMANDS and SIZES hold fixed-point sizes: fixnums for any realistic line, but plain
vectors so that arithmetic stays exact for absurd lengths, weights and spacing too. After
SOLVE, STARTS and EXTENTS hold each segment's offset from the start of the line and its length,
in cells."
  (constraints #() :type simple-vector :read-only t)
  (flex :start :type flex :read-only t)
  (spacing 0 :type fixnum :read-only t)
  (demands #() :type simple-vector :read-only t)
  (sizes #() :type simple-vector :read-only t)
  (pinned (make-array 0 :element-type 'bit) :type simple-bit-vector :read-only t)
  (starts (make-array 0 :element-type 'fixnum) :type fixnum-vector :read-only t)
  (extents (make-array 0 :element-type 'fixnum) :type fixnum-vector :read-only t))

(defun make-solver (constraints flex spacing)
  "Return a solver for CONSTRAINTS, a simple-vector of constraints, placed according to FLEX
with SPACING cells between segments."
  (check-type flex flex)
  (check-type spacing fixnum)
  (let ((count (length constraints)))
    (flet ((fixnums ()
             (make-array count :element-type 'fixnum :initial-element 0)))
      (%make-solver constraints flex spacing
                    (make-array count :initial-element 0)
                    (make-array count :initial-element 0)
                    (make-array count :element-type 'bit :initial-element 0)
                    (fixnums) (fixnums)))))

(declaim (inline solver-count))
(defun solver-count (solver)
  "Return the number of segments SOLVER divides a line into."
  (length (solver-constraints solver)))

(defun solve (solver length)
  "Divide a line of LENGTH cells among SOLVER's segments as described above: set element I of
(SOLVER-STARTS SOLVER) to segment I's offset from the start of the line and element I of
(SOLVER-EXTENTS SOLVER) to its length, both in cells. Return SOLVER."
  (check-type length (and fixnum unsigned-byte))
  (let* ((count (solver-count solver))
         (reserved (reserved-spacing (solver-flex solver) (solver-spacing solver) count))
         (fits (<= reserved length)))
    (when (plusp count)
      (fill (solver-sizes solver) 0)
      (let ((excess (if fits (allocate solver length (- length reserved)) 0)))
        (place solver length
               (* +scale+ (if fits reserved length))
               (assign-excess solver excess))))
    solver))

;;; Spacers

(defun spacer-units (flex)
  "Return as two values how FLEX weighs each edge spacer and each spacer between segments when
it shares out spacer space."
  (ecase flex
    ((:start :end :center :legacy :space-between) (values 0 1))
    (:space-evenly (values 1 1))
    (:space-around (values 1 2))))

(defun reserved-spacing (flex spacing count)
  "Return the cells that FLEX reserves for spacers around COUNT segments with SPACING."
  (multiple-value-bind (edge middle) (spacer-units flex)
    (* (+ edge edge (* (max 0 (1- count)) middle))
       (if (zerop edge) spacing (max 0 spacing)))))

(defun spacer-offset (flex count index shared excess)
  "Return the fixed-point space taken by the spacers before segment INDEX of COUNT: SHARED
spread over the spacers by FLEX's units, plus the part of EXCESS that FLEX puts there."
  (multiple-value-bind (edge middle) (spacer-units flex)
    (let ((units (+ edge edge (* (1- count) middle)))
          (shared (if (member flex '(:space-between :space-evenly :space-around))
                      (+ shared excess)
                      shared)))
      (+ (ecase flex
           ((:start :legacy :space-between :space-evenly :space-around) 0)
           (:end excess)
           (:center (floor excess 2)))
         (if (zerop units)
             0
             (floor (* shared (+ edge (* index middle))) units))))))

;;; Sizes

(defun demand (constraint length cap)
  "Return the fixed-point size that CONSTRAINT asks for in a line of LENGTH cells, at most CAP."
  (let ((value (constraint-value constraint)))
    (min cap
         (ecase (constraint-kind constraint)
           ((:min :length :max) (* value +scale+))
           (:percentage (floor (* value length +scale+) 100))
           (:ratio (floor (* value length +scale+) (constraint-denominator constraint)))
           (:fill 0)))))

(defun allocate (solver length available)
  "Size SOLVER's segments in fixed point within AVAILABLE cells of a line of LENGTH cells
(steps 2 and 3). Return the fixed-point space left unused."
  (let ((constraints (solver-constraints solver))
        (demands (solver-demands solver))
        (remaining (* available +scale+))
        (legacy (eq (solver-flex solver) :legacy)))
    (dotimes (i (length constraints))
      (setf (svref demands i) (demand (svref constraints i) length remaining)))
    (dolist (kind '(:min :length :percentage :ratio :max))
      (setf remaining (allocate-kind solver kind remaining legacy)))
    (grow solver remaining legacy)))

(defun allocate-kind (solver kind remaining legacy)
  "Grant the constraints of KIND their demands out of REMAINING and return what is left. If they
ask for more, they share REMAINING: in order when LEGACY, otherwise as evenly as possible."
  (let ((total (loop for constraint across (solver-constraints solver)
                     for demand across (solver-demands solver)
                     when (eq kind (constraint-kind constraint))
                       sum demand)))
    (cond ((<= total remaining)
           (grant-in-order solver kind total)
           (- remaining total))
          (legacy
           (grant-in-order solver kind remaining)
           0)
          (t
           (pin-small-demands solver kind remaining)
           (share-equally solver kind remaining)
           0))))

(defun grant-in-order (solver kind budget)
  "Grant each constraint of KIND its demand, in order, while BUDGET lasts."
  (loop for constraint across (solver-constraints solver)
        for i from 0
        when (eq kind (constraint-kind constraint))
          do (let ((grant (min budget (svref (solver-demands solver) i))))
               (setf (svref (solver-sizes solver) i) grant)
               (decf budget grant))))

(defun unpinned (solver kind budget)
  "Return the number of unpinned constraints of KIND, and what is left of BUDGET after the
demands of the pinned ones."
  (let ((count 0)
        (free budget))
    (loop for constraint across (solver-constraints solver)
          for i from 0
          when (eq kind (constraint-kind constraint))
            do (if (zerop (sbit (solver-pinned solver) i))
                   (incf count)
                   (decf free (svref (solver-demands solver) i))))
    (values count free)))

(defun pin-small-demands (solver kind budget)
  "Pin, and grant in full, every constraint of KIND whose demand is no more than an equal share
of what BUDGET leaves for the unpinned ones, until no more qualify."
  (let ((constraints (solver-constraints solver))
        (demands (solver-demands solver))
        (pinned (solver-pinned solver)))
    (fill pinned 0)
    (loop
      (multiple-value-bind (count free) (unpinned solver kind budget)
        (let ((changed nil))
          (dotimes (i (length constraints))
            (when (and (eq kind (constraint-kind (svref constraints i)))
                       (zerop (sbit pinned i))
                       (<= (* count (svref demands i)) free))
              (setf (sbit pinned i) 1
                    (svref (solver-sizes solver) i) (svref demands i)
                    changed t)))
          (unless changed
            (return)))))))

(defun share-equally (solver kind budget)
  "Divide what the pinned constraints of KIND leave of BUDGET equally among the unpinned ones."
  (multiple-value-bind (count free) (unpinned solver kind budget)
    (let ((k 0))
      (declare (fixnum k))
      (loop for constraint across (solver-constraints solver)
            for i from 0
            when (and (eq kind (constraint-kind constraint))
                      (zerop (sbit (solver-pinned solver) i)))
              do (setf (svref (solver-sizes solver) i)
                       (- (floor (* free (1+ k)) count) (floor (* free k) count)))
                 (incf k)))))

;;; Growth

(defun growth-weight (constraint legacy uniform)
  "Return CONSTRAINT's weight when sharing space left after step 2, or NIL if it doesn't grow.
With UNIFORM, every growing constraint has weight 1."
  (let ((weight (ecase (constraint-kind constraint)
                  (:fill (constraint-value constraint))
                  (:min (if legacy nil 1))
                  ((:length :percentage :ratio :max) nil))))
    (if (and weight uniform) 1 weight)))

(defun grow (solver remaining legacy)
  "Share REMAINING among the growing constraints in proportion to their weights, keeping each at
least at the size it already has (step 3). Return what is left: 0 if anything grows."
  (let ((growers 0)
        (weights 0))
    (loop for constraint across (solver-constraints solver)
          do (let ((weight (growth-weight constraint legacy nil)))
               (when weight
                 (incf growers)
                 (incf weights weight))))
    (cond ((zerop growers) remaining)
          (t (let ((uniform (zerop weights)))
               (pin-floors solver legacy uniform remaining)
               (share-by-weight solver legacy uniform remaining))
             0))))

(defun unpinned-growth (solver legacy uniform remaining)
  "Return the total weight of the unpinned growing constraints, and the space they share:
REMAINING plus their current sizes."
  (let ((weights 0)
        (free remaining))
    (loop for constraint across (solver-constraints solver)
          for i from 0
          do (let ((weight (growth-weight constraint legacy uniform)))
               (when (and weight (zerop (sbit (solver-pinned solver) i)))
                 (incf weights weight)
                 (incf free (svref (solver-sizes solver) i)))))
    (values weights free)))

(defun pin-floors (solver legacy uniform remaining)
  "Pin the growing constraints whose share would be smaller than their current size, until no
more qualify, and those of weight 0. Pinned constraints keep their current size."
  (let ((constraints (solver-constraints solver))
        (pinned (solver-pinned solver)))
    (fill pinned 0)
    (dotimes (i (length constraints))
      (when (eql 0 (growth-weight (svref constraints i) legacy uniform))
        (setf (sbit pinned i) 1)))
    (loop
      (multiple-value-bind (weights free) (unpinned-growth solver legacy uniform remaining)
        (let ((changed nil))
          (dotimes (i (length constraints))
            (let ((weight (growth-weight (svref constraints i) legacy uniform)))
              (when (and weight
                         (zerop (sbit pinned i))
                         (< (* free weight) (* weights (svref (solver-sizes solver) i))))
                (setf (sbit pinned i) 1
                      changed t))))
          (unless changed
            (return)))))))

(defun share-by-weight (solver legacy uniform remaining)
  "Divide the space of the unpinned growing constraints among them in proportion to weight."
  (multiple-value-bind (weights free) (unpinned-growth solver legacy uniform remaining)
    (let ((before 0))
      (loop for constraint across (solver-constraints solver)
            for i from 0
            do (let ((weight (growth-weight constraint legacy uniform)))
                 (when (and weight (zerop (sbit (solver-pinned solver) i)))
                   (setf (svref (solver-sizes solver) i)
                         (- (floor (* free (+ before weight)) weights)
                            (floor (* free before) weights)))
                   (incf before weight)))))))

;;; Excess and placement

(defun legacy-target (constraints)
  "Return the index of the segment that takes the excess under :legacy flex."
  (dolist (kind '(:min :ratio :percentage :length :max))
    (let ((index (position kind constraints :key #'constraint-kind :from-end t)))
      (when index
        (return index)))))

(defun assign-excess (solver excess)
  "Give EXCESS to a segment and return 0 where the flex mode says so (step 4); otherwise
return EXCESS, which goes to the spacers."
  (let ((flex (solver-flex solver))
        (sizes (solver-sizes solver)))
    (cond ((zerop excess) 0)
          ((eq flex :legacy)
           (incf (svref sizes (legacy-target (solver-constraints solver))) excess)
           0)
          ((and (eq flex :space-between) (= 1 (solver-count solver)))
           (incf (svref sizes 0) excess)
           0)
          (t excess))))

(defun cell (position length)
  "Return the cell boundary nearest to the fixed-point POSITION, rounding halves up, kept
within 0 and LENGTH."
  (max 0 (min length (floor (+ position (floor +scale+ 2)) +scale+))))

(defun place (solver length shared excess)
  "Turn the fixed-point segment sizes into cell offsets and extents (step 5). SHARED is the
spacer space that the flex mode spreads by its units and EXCESS the space it adds to spacers."
  (let ((flex (solver-flex solver))
        (count (solver-count solver))
        (sizes (solver-sizes solver))
        (before 0))
    (dotimes (i count)
      (let* ((start (+ before (spacer-offset flex count i shared excess)))
             (from (cell start length))
             (to (cell (+ start (svref sizes i)) length)))
        (setf (aref (solver-starts solver) i) from
              (aref (solver-extents solver) i) (max 0 (- to from)))
        (incf before (svref sizes i))))))
