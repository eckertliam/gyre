;;;; layout.lisp — tests for tui/layout: the split API, caching, and ratatui's tables for :legacy
;;;; flex. The helpers that run ported ratatui tables are shared with the other layout tests.

(defpackage #:tui/tests/layout
  (:use #:cl)
  (:import-from #:tui/tests/harness #:deftest #:is #:signals)
  (:import-from #:tui/layout
                #:make-rect
                #:rect-x
                #:rect-y
                #:rect-width
                #:rect-height
                #:make-layout
                #:layout-direction
                #:layout-constraints
                #:split
                #:split-area
                #:invalid-constraint
                #:invalid-constraint-designator)
  (:export #:check-letters
           #:check-spans
           #:check-spacers
           #:check-flex-widths))

(in-package #:tui/tests/layout)

;;; Running tables of cases ported from ratatui's layout tests

(defun split-line (flex spacing width constraints)
  "Split a WIDTH x 1 rect horizontally and return the (x width) of each area."
  (map 'list (lambda (area) (list (rect-x area) (rect-width area)))
       (split-area (make-rect 0 0 width 1) :horizontal constraints :flex flex :spacing spacing)))

(defun split-column (flex spacing width constraints)
  "Split vertically an offset rect whose inner area, inside margins, is 3 x WIDTH, and return
the (y height) of each area relative to the inner area, or :wrong-columns for an area that
doesn't span the inner area's columns."
  (map 'list (lambda (area)
               (if (and (= 6 (rect-x area)) (= 3 (rect-width area)))
                   (list (- (rect-y area) 7) (rect-height area))
                   :wrong-columns))
       (split-area (make-rect 5 5 5 (+ width 4)) :vertical constraints
                   :flex flex :spacing spacing :horizontal-margin 1 :vertical-margin 2)))

(defun check-spans (cases)
  "Check cases (flex spacing width constraints expected), where EXPECTED lists the (start
length) of each area along a line of WIDTH cells: as a horizontal split, and as a vertical
split of an offset rect with margins."
  (dolist (case cases)
    (destructuring-bind (flex spacing width constraints expected) case
      (declare (ignore expected))
      (is (equal case (list flex spacing width constraints
                            (split-line flex spacing width constraints))))
      (is (equal case (list flex spacing width constraints
                            (split-column flex spacing width constraints)))))))

(defun letters (flex width constraints)
  "Show a horizontal split of a WIDTH x 1 rect as ratatui's tests do: a string in which the cells
of the Ith area hold the Ith letter of the alphabet, later areas over earlier ones, and the
other cells are spaces."
  (let ((line (make-string width :initial-element #\Space)))
    (loop for (x extent) in (split-line flex 0 width constraints)
          for letter across "abcdefghijklmnopqrstuvwxyz"
          do (fill line letter :start x :end (+ x extent)))
    line))

(defun check-letters (cases)
  "Check cases (flex width constraints expected), where EXPECTED is what LETTERS shows."
  (dolist (case cases)
    (destructuring-bind (flex width constraints expected) case
      (declare (ignore expected))
      (is (equal case (list flex width constraints (letters flex width constraints)))))))

(defun spacers (flex spacing width constraints)
  "Return the (x width) of the spacers before, between and after the areas of a horizontal
split, as ratatui's split_with_spacers reports them: the spacer between overlapping areas is
empty and starts where the earlier area ends."
  (let ((end 0)
        (spacers '()))
    (loop for (x extent) in (split-line flex spacing width constraints)
          do (push (list end (max 0 (- x end))) spacers)
             (setf end (+ x extent)))
    (push (list end (- width end)) spacers)
    (nreverse spacers)))

(defun check-spacers (cases)
  "Check cases (flex spacing width constraints expected), where EXPECTED lists the (x width) of
the spacers around the areas of a horizontal split."
  (dolist (case cases)
    (destructuring-bind (flex spacing width constraints expected) case
      (declare (ignore expected))
      (is (equal case (list flex spacing width constraints
                            (spacers flex spacing width constraints)))))))

(defun check-flex-widths (cases)
  "Check cases (constraints widths): under every flex mode but :legacy, splitting a 100 x 1 rect
horizontally gives areas of WIDTHS."
  (dolist (case cases)
    (destructuring-bind (constraints widths) case
      (declare (ignore widths))
      (dolist (flex '(:start :end :center :space-between :space-around :space-evenly))
        (is (equal (list flex case)
                   (list flex (list constraints
                                    (mapcar #'second (split-line flex 0 100 constraints))))))))))

;;; The layout API

(defun areas (&rest rects)
  "Return RECTS, each a list (x y width height), as a vector of rects."
  (map 'vector (lambda (rect) (apply #'make-rect rect)) rects))

(deftest layout-defaults
  (let ((layout (make-layout :constraints '((:length 2) (:fill)))))
    (is (eq :vertical (layout-direction layout)))
    (is (equal '((:length 2) (:fill)) (layout-constraints layout)))
    (is (equalp (areas '(0 0 4 2) '(0 2 4 8)) (split layout (make-rect 0 0 4 10))))
    ;; :start flex: unused space goes after the areas.
    (is (equalp (areas '(0 0 4 2)) (split-area (make-rect 0 0 4 10) :vertical '((:length 2)))))))

(deftest layout-directions
  (let ((constraints '((:length 5) (:fill 1)))
        (area (make-rect 0 0 10 10)))
    (is (equalp (areas '(0 0 5 10) '(5 0 5 10)) (split-area area :horizontal constraints)))
    (is (equalp (areas '(0 0 10 5) '(0 5 10 5)) (split-area area :vertical constraints)))
    (is (equalp (areas '(2 2 10 5) '(2 7 10 5))
                (split-area (make-rect 2 2 10 10) :vertical constraints)))))

(deftest layout-margins
  (let ((area (make-rect 0 0 10 10)))
    (is (equalp (areas '(2 2 6 6)) (split-area area :vertical '((:fill)) :margin 2)))
    (is (equalp (areas '(2 0 6 10)) (split-area area :vertical '((:fill)) :horizontal-margin 2)))
    (is (equalp (areas '(0 2 10 6)) (split-area area :vertical '((:fill)) :vertical-margin 2)))
    (is (equalp (areas '(1 3 8 4))
                (split-area area :vertical '((:fill)) :margin 1 :vertical-margin 3)))
    ;; Margins that don't fit leave empty areas inside the rect.
    (is (equalp (areas '(6 6 0 0) '(6 6 0 0))
                (split-area area :horizontal '((:fill) (:fill)) :margin 6)))))

(deftest layout-ratatui-examples
  ;; Examples from the documentation of ratatui's Layout and Rect.
  (is (equalp (areas '(0 0 3 2) '(3 0 6 2))
              (split-area (make-rect 0 0 9 2) :horizontal '((:ratio 1 3) (:ratio 2 3)))))
  (is (equalp (areas '(0 0 3 2) '(4 0 6 2))
              (split-area (make-rect 0 0 10 2) :horizontal '((:ratio 1 3) (:ratio 2 3))
                          :spacing 1)))
  (is (equalp (areas '(0 0 10 2) '(0 2 10 2) '(0 4 10 2) '(0 6 10 2) '(0 8 10 2))
              (split-area (make-rect 0 0 10 10) :vertical
                          '((:percentage 20) (:ratio 1 5) (:length 2) (:min 2) (:max 2)))))
  (is (equalp (areas '(0 0 3 10) '(3 0 7 10))
              (split-area (make-rect 0 0 10 10) :horizontal '((:length 3) (:min 0)))))
  (is (equalp (areas '(1 0 3 5))
              (split-area (make-rect 0 0 5 5) :horizontal '((:length 3)) :flex :center)))
  (is (equalp (areas '(0 2 5 1))
              (split-area (make-rect 0 0 5 5) :vertical '((:length 1)) :flex :center))))

(deftest layout-edge-cases
  ;; ratatui: layout::tests::split::edge_cases and vertical_split_by_height.
  (is (equalp (areas '(0 0 1 1) '(0 1 1 0) '(0 1 1 0))
              (split-area (make-rect 0 0 1 1) :vertical
                          '((:percentage 50) (:percentage 50) (:min 0)))))
  (is (equalp (areas '(0 0 1 0) '(0 0 1 1) '(0 1 1 0))
              (split-area (make-rect 0 0 1 1) :vertical '((:max 1) (:percentage 99) (:min 0)))))
  (is (equalp (areas '(0 0 1 1) '(1 0 0 1) '(1 0 0 1))
              (split-area (make-rect 0 0 1 1) :horizontal '((:min 1) (:length 0) (:min 1)))))
  (is (equalp (areas '(0 0 0 1) '(0 0 4 1) '(4 0 0 1) '(4 0 3 1))
              (split-area (make-rect 0 0 7 1) :horizontal
                          '((:length 3) (:min 4) (:length 1) (:min 4)))))
  (is (equalp (areas '(2 2 10 1) '(2 3 10 5) '(2 8 10 4))
              (split-area (make-rect 2 2 10 10) :vertical
                          '((:percentage 10) (:max 5) (:min 1))))))

(deftest layout-degenerate-areas
  (is (equalp #() (split-area (make-rect 3 4 10 10) :vertical '())))
  (is (equalp (areas '(3 4 0 0) '(3 4 0 0))
              (split-area (make-rect 3 4 0 0) :horizontal '((:length 5) (:fill)))))
  (is (equalp (areas '(3 4 0 7) '(3 4 0 7))
              (split-area (make-rect 3 4 0 7) :horizontal '((:min 5) (:percentage 50)))))
  (is (equalp (areas '(3 4 6 0) '(3 4 6 0))
              (split-area (make-rect 3 4 6 0) :vertical '((:min 5) (:ratio 1 2))
                          :flex :space-around :spacing 2))))

(deftest layout-huge-values
  ;; Values far beyond any terminal still give areas inside the rect.
  (let ((huge most-positive-fixnum)
        (area (make-rect 0 0 80 1)))
    (is (equalp (areas '(0 0 40 1) '(40 0 40 1))
                (split-area area :horizontal `((:fill ,huge) (:fill ,huge)))))
    (is (equalp (areas '(0 0 80 1) '(80 0 0 1))
                (split-area area :horizontal `((:length ,huge) (:percentage ,huge)))))
    (is (equalp (areas '(0 0 0 1) '(0 0 80 1))
                (split-area area :horizontal `((:ratio ,huge 1) (:min ,huge)))))
    (is (equalp (areas '(0 0 0 1) '(80 0 0 1))
                (split-area area :horizontal '((:length 10) (:length 10)) :spacing huge)))
    (is (equalp (areas '(0 0 10 1) '(0 0 0 1))
                (split-area area :horizontal '((:length 10) (:length 10))
                            :spacing most-negative-fixnum)))
    (is (equalp (areas '(80 1 0 0))
                (split-area area :horizontal '((:fill)) :margin huge)))))

;;; Constraint designators

(defun constraint-error (designator)
  "Return (:invalid D) if making a layout with DESIGNATOR signals INVALID-CONSTRAINT reporting
the designator D, and :valid if it succeeds."
  (handler-case (progn (make-layout :constraints (list designator)) :valid)
    (invalid-constraint (condition)
      (list :invalid (invalid-constraint-designator condition)))))

(deftest constraint-designators
  (dolist (designator '((:length 0) (:length 65535) (:min 3) (:max 3) (:percentage 250)
                        (:ratio 0 1) (:ratio 7 3) (:fill) (:fill 0) (:fill 4)))
    (is (eq :valid (constraint-error designator))))
  (dolist (designator '((:length) (:length -1) (:length 1.5) (:length 1 2) (:length . 1)
                        (:min "3") (:max nil) (:percentage 1/2) (:ratio 1) (:ratio 1 0)
                        (:ratio -1 2) (:ratio 1 2 3) (:fill 1 2) (:fill -1) (:fill . 1)
                        (:size 3) (length 3) :fill 5 () "length"))
    (is (equal (list :invalid designator) (constraint-error designator))))
  (is (subtypep 'invalid-constraint 'error))
  (signals invalid-constraint (split-area (make-rect 0 0 1 1) :vertical '((:fill) (:half)))))

(deftest constraint-fill-weight-defaults-to-one
  (let ((area (make-rect 0 0 9 1)))
    (is (equalp (split-area area :horizontal '((:fill 1) (:fill 2)))
                (split-area area :horizontal '((:fill) (:fill 2)))))))

(deftest layout-options-are-checked
  (signals type-error (make-layout :direction :diagonal))
  (signals type-error (make-layout :flex :stretch))
  (signals type-error (make-layout :spacing 1.5))
  (signals type-error (make-layout :margin -1))
  (signals type-error (make-layout :vertical-margin -2))
  (signals type-error (make-layout :constraints #((:fill)))))

;;; Caching

(defun bytes-consed-by-splits (layout area count)
  "Return the bytes allocated while splitting AREA with LAYOUT COUNT times."
  (let ((before (sb-ext:get-bytes-consed)))
    (loop repeat count
          do (split layout area))
    (- (sb-ext:get-bytes-consed) before)))

(deftest split-caches-its-result
  (let* ((layout (make-layout :direction :horizontal
                              :constraints '((:length 3) (:fill) (:percentage 20))))
         (areas (split layout (make-rect 0 0 80 24))))
    (is (eq areas (split layout (make-rect 0 0 80 24))))
    (let ((wider (split layout (make-rect 0 0 100 24))))
      (is (not (eq areas wider)))
      (is (eq wider (split layout (make-rect 0 0 100 24)))))
    (is (equalp areas (split layout (make-rect 0 0 80 24))))))

(deftest cached-split-allocates-nothing
  (let ((layout (make-layout :constraints '((:length 1) (:fill) (:length 1)) :margin 1)))
    (split layout (make-rect 0 0 80 24))
    (is (= 0 (bytes-consed-by-splits layout (make-rect 0 0 80 24) 1000)))))

;;; ratatui's tables for :legacy flex

(deftest constraint-length
  ;; ratatui: layout::tests::split::constraint_length
  (check-spans
   '((:legacy 0 100 ((:length 25) (:min 100)) ((0 0) (0 100)))
     (:legacy 0 100 ((:length 25) (:min 0)) ((0 25) (25 75)))
     (:legacy 0 100 ((:length 25) (:max 0)) ((0 100) (100 0)))
     (:legacy 0 100 ((:length 25) (:max 100)) ((0 25) (25 75)))
     (:legacy 0 100 ((:length 25) (:percentage 25)) ((0 25) (25 75)))
     (:legacy 0 100 ((:percentage 25) (:length 25)) ((0 75) (75 25)))
     (:legacy 0 100 ((:length 25) (:ratio 1 4)) ((0 25) (25 75)))
     (:legacy 0 100 ((:ratio 1 4) (:length 25)) ((0 75) (75 25)))
     (:legacy 0 100 ((:length 25) (:length 25)) ((0 25) (25 75)))
     (:legacy 0 100 ((:length 25) (:length 25) (:length 25)) ((0 25) (25 25) (50 50)))
     (:legacy 0 100 ((:length 15) (:length 35) (:length 25)) ((0 15) (15 35) (50 50)))
     (:legacy 0 100 ((:length 25) (:length 25) (:length 25)) ((0 25) (25 25) (50 50))))))

(deftest table-length
  ;; ratatui: layout::tests::split::table_length
  (check-spans
   '((:start 1 7 ((:length 4) (:length 4)) ((0 3) (4 3)))
     (:start 1 4 ((:length 4) (:length 4)) ((0 2) (3 1))))))

(deftest length-is-higher-priority
  ;; ratatui: layout::tests::split::length_is_higher_priority
  (check-spans
   '((:legacy 0 100 ((:min 25) (:length 25) (:max 25)) ((0 50) (50 25) (75 25)))
     (:legacy 0 100 ((:max 25) (:length 25) (:min 25)) ((0 25) (25 25) (50 50)))
     (:legacy 0 100 ((:length 33) (:length 33) (:length 33)) ((0 33) (33 33) (66 34)))
     (:legacy 0 100 ((:length 25) (:length 25) (:length 25)) ((0 25) (25 25) (50 50)))
     (:legacy 0 100 ((:percentage 25) (:length 25) (:ratio 1 4)) ((0 25) (25 25) (50 50)))
     (:legacy 0 100 ((:length 25) (:ratio 1 4) (:percentage 25)) ((0 25) (25 50) (75 25)))
     (:legacy 0 100 ((:ratio 1 4) (:length 25) (:percentage 25)) ((0 50) (50 25) (75 25)))
     (:legacy 0 100 ((:ratio 1 4) (:percentage 25) (:length 25)) ((0 50) (50 25) (75 25)))
     (:legacy 0 100 ((:length 100) (:length 1) (:min 20)) ((0 80) (80 0) (80 20)))
     (:legacy 0 100 ((:min 20) (:length 1) (:length 100)) ((0 20) (20 1) (21 79)))
     (:legacy 0 100 ((:fill 1) (:length 10) (:fill 1)) ((0 45) (45 10) (55 45)))
     (:legacy 0 100 ((:fill 1) (:length 10) (:fill 2)) ((0 30) (30 10) (40 60)))
     (:legacy 0 100 ((:fill 1) (:length 10) (:fill 4)) ((0 18) (18 10) (28 72)))
     (:legacy 0 100 ((:fill 1) (:length 10) (:fill 5)) ((0 15) (15 10) (25 75)))
     (:legacy 0 100 ((:length 25) (:length 25) (:length 25)) ((0 25) (25 25) (50 50)))
     (:legacy 0 100 ((:length 25) (:length 25) (:length 25)) ((0 25) (25 25) (50 50))))))

(deftest fixed-with-50-width
  ;; ratatui: layout::tests::split::fixed_with_50_width
  (check-spans
   '((:legacy 0 50 ((:fill 1) (:length 10) (:fill 2)) ((0 13) (13 10) (23 27)))
     (:legacy 0 50 ((:length 10) (:fill 2) (:fill 1)) ((0 10) (10 27) (37 13))))))

(deftest fill-constraints
  ;; ratatui: layout::tests::split::fill
  (check-spans
   '((:legacy 0 100 ((:fill 1) (:fill 2) (:fill 1) (:fill 1)) ((0 20) (20 40) (60 20) (80 20)))
     (:legacy 0 100 ((:fill 1) (:fill 2) (:fill 3) (:fill 4)) ((0 10) (10 20) (30 30) (60 40)))
     (:legacy 0 100 ((:fill 4) (:fill 3) (:fill 2) (:fill 1)) ((0 40) (40 30) (70 20) (90 10)))
     (:legacy 0 100 ((:fill 1) (:fill 3) (:fill 2) (:fill 4)) ((0 10) (10 30) (40 20) (60 40)))
     (:legacy 0 100 ((:fill 1) (:fill 3) (:length 50) (:fill 2) (:fill 4))
      ((0 5) (5 15) (20 50) (70 10) (80 20)))
     (:legacy 0 100 ((:fill 1) (:fill 3) (:percentage 50) (:fill 2) (:fill 4))
      ((0 5) (5 15) (20 50) (70 10) (80 20)))
     (:legacy 0 100 ((:fill 1) (:fill 3) (:min 50) (:fill 2) (:fill 4))
      ((0 5) (5 15) (20 50) (70 10) (80 20)))
     (:legacy 0 100 ((:fill 1) (:fill 3) (:max 50) (:fill 2) (:fill 4))
      ((0 5) (5 15) (20 50) (70 10) (80 20)))
     (:legacy 0 100 ((:fill 0) (:fill 1) (:fill 0)) ((0 0) (0 100) (100 0)))
     (:legacy 0 100 ((:fill 0) (:length 1) (:fill 0)) ((0 50) (50 1) (51 49)))
     (:legacy 0 100 ((:fill 0) (:percentage 1) (:fill 0)) ((0 50) (50 1) (51 49)))
     (:legacy 0 100 ((:fill 0) (:min 1) (:fill 0)) ((0 50) (50 1) (51 49)))
     (:legacy 0 100 ((:fill 0) (:max 1) (:fill 0)) ((0 50) (50 1) (51 49)))
     (:legacy 0 100 ((:fill 0) (:fill 2) (:fill 0) (:fill 1)) ((0 0) (0 67) (67 0) (67 33)))
     (:legacy 0 100 ((:fill 0) (:fill 2) (:percentage 20)) ((0 0) (0 80) (80 20)))
     (:legacy 0 100 ((:fill 0) (:fill 0) (:percentage 20)) ((0 40) (40 40) (80 20)))
     (:legacy 0 100 ((:fill 0) (:ratio 1 5)) ((0 80) (80 20)))
     (:legacy 0 100 ((:fill 0) (:fill 65535)) ((0 0) (0 100)))
     (:legacy 0 100 ((:fill 65535) (:fill 0)) ((0 100) (100 0)))
     (:legacy 0 100 ((:fill 0) (:percentage 20)) ((0 80) (80 20)))
     (:legacy 0 100 ((:fill 1) (:percentage 20)) ((0 80) (80 20)))
     (:legacy 0 100 ((:fill 65535) (:percentage 20)) ((0 80) (80 20)))
     (:legacy 0 100 ((:fill 65535) (:fill 0) (:percentage 20)) ((0 80) (80 0) (80 20)))
     (:legacy 0 100 ((:fill 0) (:length 20)) ((0 80) (80 20)))
     (:legacy 0 100 ((:fill 0) (:min 20)) ((0 80) (80 20)))
     (:legacy 0 100 ((:fill 0) (:max 20)) ((0 80) (80 20)))
     (:legacy 0 100 ((:fill 1) (:fill 1) (:fill 1) (:min 30) (:length 50))
      ((0 7) (7 6) (13 7) (20 30) (50 50)))
     (:legacy 0 100 ((:fill 1) (:fill 1) (:fill 1) (:length 50) (:length 50))
      ((0 0) (0 0) (0 0) (0 50) (50 50)))
     (:legacy 0 100 ((:fill 1) (:fill 1) (:fill 1) (:length 75) (:length 50))
      ((0 0) (0 0) (0 0) (0 75) (75 25)))
     (:legacy 0 100 ((:fill 1) (:fill 1) (:fill 1) (:min 50) (:max 50))
      ((0 0) (0 0) (0 0) (0 50) (50 50)))
     (:legacy 0 100 ((:fill 1) (:fill 1) (:fill 1) (:ratio 1 1)) ((0 0) (0 0) (0 0) (0 100)))
     (:legacy 0 100 ((:fill 1) (:fill 1) (:fill 1) (:percentage 100))
      ((0 0) (0 0) (0 0) (0 100))))))

(deftest percentage-parameterized
  ;; ratatui: layout::tests::split::percentage_parameterized
  (check-spans
   '((:legacy 0 100 ((:min 0) (:percentage 20)) ((0 80) (80 20)))
     (:legacy 0 100 ((:max 0) (:percentage 20)) ((0 0) (0 100))))))

(deftest min-max
  ;; ratatui: layout::tests::split::min_max
  (check-spans
   '((:legacy 0 100 ((:max 100) (:min 0)) ((0 100) (100 0)))
     (:legacy 0 100 ((:min 0) (:max 100)) ((0 0) (0 100)))
     (:legacy 0 100 ((:length 65535) (:min 10)) ((0 90) (90 10)))
     (:legacy 0 100 ((:min 10) (:length 65535)) ((0 10) (10 90)))
     (:legacy 0 100 ((:length 0) (:max 10)) ((0 90) (90 10)))
     (:legacy 0 100 ((:max 10) (:length 0)) ((0 10) (10 90))))))

(deftest constraint-priority
  ;; ratatui: layout::tests::split::constraint_specification_tests_for_priority
  (check-spans
   '((:legacy 0 100 ((:length 25) (:length 25)) ((0 25) (25 75)))
     (:legacy 0 100 ((:length 25) (:percentage 25)) ((0 25) (25 75)))
     (:legacy 0 100 ((:percentage 25) (:length 25)) ((0 75) (75 25)))
     (:legacy 0 100 ((:min 25) (:percentage 25)) ((0 75) (75 25)))
     (:legacy 0 100 ((:percentage 25) (:min 25)) ((0 25) (25 75)))
     (:legacy 0 100 ((:min 25) (:percentage 100)) ((0 25) (25 75)))
     (:legacy 0 100 ((:percentage 100) (:min 25)) ((0 75) (75 25)))
     (:legacy 0 100 ((:max 75) (:percentage 75)) ((0 25) (25 75)))
     (:legacy 0 100 ((:percentage 75) (:max 75)) ((0 75) (75 25)))
     (:legacy 0 100 ((:max 25) (:percentage 25)) ((0 25) (25 75)))
     (:legacy 0 100 ((:percentage 25) (:max 25)) ((0 75) (75 25)))
     (:legacy 0 100 ((:length 25) (:ratio 1 4)) ((0 25) (25 75)))
     (:legacy 0 100 ((:ratio 1 4) (:length 25)) ((0 75) (75 25)))
     (:legacy 0 100 ((:percentage 25) (:ratio 1 4)) ((0 25) (25 75)))
     (:legacy 0 100 ((:ratio 1 4) (:percentage 25)) ((0 75) (75 25)))
     (:legacy 0 100 ((:ratio 1 4) (:fill 25)) ((0 25) (25 75)))
     (:legacy 0 100 ((:fill 25) (:ratio 1 4)) ((0 75) (75 25))))))

(deftest split-does-not-keep-the-callers-rect
  ;; A caller may pass a stack-allocated rect; the cache must hold its own copy.
  (let ((layout (make-layout :constraints '((:length 1) (:fill 1)))))
    (let ((area (make-rect 0 0 10 10)))
      (declare (dynamic-extent area))
      (split layout area))
    (is (not (eq (tui/layout::layout-cached-area layout) nil)))
    (is (tui/layout:rect= (make-rect 0 0 10 10) (tui/layout::layout-cached-area layout)))
    (is (eq (split layout (make-rect 0 0 10 10)) (split layout (make-rect 0 0 10 10))))))
