;;;; layout-flex.lisp — ratatui's layout tables for the flex modes and spacing, including
;;;; negative spacing (overlap) and spacing that doesn't fit.

(defpackage #:tui/tests/layout-flex
  (:use #:cl)
  (:import-from #:tui/tests/harness #:deftest)
  (:import-from #:tui/tests/layout
                #:check-letters
                #:check-spans
                #:check-spacers
                #:check-flex-widths))

(in-package #:tui/tests/layout-flex)

(deftest length-is-higher-priority-in-flex
  ;; ratatui: layout::tests::split::length_is_higher_priority_in_flex
  (check-flex-widths
   '((((:min 25) (:length 25) (:max 25)) (50 25 25))
     (((:max 25) (:length 25) (:min 25)) (25 25 50))
     (((:length 33) (:length 33) (:length 33)) (33 33 33))
     (((:length 25) (:length 25) (:length 25)) (25 25 25))
     (((:percentage 25) (:length 25) (:ratio 1 4)) (25 25 25))
     (((:length 25) (:ratio 1 4) (:percentage 25)) (25 25 25))
     (((:ratio 1 4) (:length 25) (:percentage 25)) (25 25 25))
     (((:ratio 1 4) (:percentage 25) (:length 25)) (25 25 25))
     (((:length 100) (:length 1) (:min 20)) (79 1 20))
     (((:min 20) (:length 1) (:length 100)) (20 1 79))
     (((:fill 1) (:length 10) (:fill 1)) (45 10 45))
     (((:fill 1) (:length 10) (:fill 2)) (30 10 60))
     (((:fill 1) (:length 10) (:fill 4)) (18 10 72))
     (((:fill 1) (:length 10) (:fill 5)) (15 10 75))
     (((:length 25) (:length 25) (:length 25)) (25 25 25)))))

(deftest letters-percentage-start
  ;; ratatui: layout::tests::split::percentage_start
  (check-letters
   '((:start 10 ((:percentage 0) (:percentage 0)) "          ")
     (:start 10 ((:percentage 0) (:percentage 25)) "bbb       ")
     (:start 10 ((:percentage 0) (:percentage 50)) "bbbbb     ")
     (:start 10 ((:percentage 0) (:percentage 100)) "bbbbbbbbbb")
     (:start 10 ((:percentage 0) (:percentage 200)) "bbbbbbbbbb")
     (:start 10 ((:percentage 10) (:percentage 0)) "a         ")
     (:start 10 ((:percentage 10) (:percentage 25)) "abbb      ")
     (:start 10 ((:percentage 10) (:percentage 50)) "abbbbb    ")
     (:start 10 ((:percentage 10) (:percentage 100)) "abbbbbbbbb")
     (:start 10 ((:percentage 10) (:percentage 200)) "abbbbbbbbb")
     (:start 10 ((:percentage 25) (:percentage 0)) "aaa       ")
     (:start 10 ((:percentage 25) (:percentage 25)) "aaabb     ")
     (:start 10 ((:percentage 25) (:percentage 50)) "aaabbbbb  ")
     (:start 10 ((:percentage 25) (:percentage 100)) "aaabbbbbbb")
     (:start 10 ((:percentage 25) (:percentage 200)) "aaabbbbbbb")
     (:start 10 ((:percentage 33) (:percentage 0)) "aaa       ")
     (:start 10 ((:percentage 33) (:percentage 25)) "aaabbb    ")
     (:start 10 ((:percentage 33) (:percentage 50)) "aaabbbbb  ")
     (:start 10 ((:percentage 33) (:percentage 100)) "aaabbbbbbb")
     (:start 10 ((:percentage 33) (:percentage 200)) "aaabbbbbbb")
     (:start 10 ((:percentage 50) (:percentage 0)) "aaaaa     ")
     (:start 10 ((:percentage 50) (:percentage 50)) "aaaaabbbbb")
     (:start 10 ((:percentage 50) (:percentage 100)) "aaaaabbbbb")
     (:start 10 ((:percentage 100) (:percentage 0)) "aaaaaaaaaa")
     (:start 10 ((:percentage 100) (:percentage 50)) "aaaaabbbbb")
     (:start 10 ((:percentage 100) (:percentage 100)) "aaaaabbbbb")
     (:start 10 ((:percentage 100) (:percentage 200)) "aaaaabbbbb"))))

(deftest letters-percentage-space-between
  ;; ratatui: layout::tests::split::percentage_spacebetween
  (check-letters
   '((:space-between 10 ((:percentage 0) (:percentage 0)) "          ")
     (:space-between 10 ((:percentage 0) (:percentage 25)) "        bb")
     (:space-between 10 ((:percentage 0) (:percentage 50)) "     bbbbb")
     (:space-between 10 ((:percentage 0) (:percentage 100)) "bbbbbbbbbb")
     (:space-between 10 ((:percentage 0) (:percentage 200)) "bbbbbbbbbb")
     (:space-between 10 ((:percentage 10) (:percentage 0)) "a         ")
     (:space-between 10 ((:percentage 10) (:percentage 25)) "a       bb")
     (:space-between 10 ((:percentage 10) (:percentage 50)) "a    bbbbb")
     (:space-between 10 ((:percentage 10) (:percentage 100)) "abbbbbbbbb")
     (:space-between 10 ((:percentage 10) (:percentage 200)) "abbbbbbbbb")
     (:space-between 10 ((:percentage 25) (:percentage 0)) "aaa       ")
     (:space-between 10 ((:percentage 25) (:percentage 25)) "aaa     bb")
     (:space-between 10 ((:percentage 25) (:percentage 50)) "aaa  bbbbb")
     (:space-between 10 ((:percentage 25) (:percentage 100)) "aaabbbbbbb")
     (:space-between 10 ((:percentage 25) (:percentage 200)) "aaabbbbbbb")
     (:space-between 10 ((:percentage 33) (:percentage 0)) "aaa       ")
     (:space-between 10 ((:percentage 33) (:percentage 25)) "aaa     bb")
     (:space-between 10 ((:percentage 33) (:percentage 50)) "aaa  bbbbb")
     (:space-between 10 ((:percentage 33) (:percentage 100)) "aaabbbbbbb")
     (:space-between 10 ((:percentage 33) (:percentage 200)) "aaabbbbbbb")
     (:space-between 10 ((:percentage 50) (:percentage 0)) "aaaaa     ")
     (:space-between 10 ((:percentage 50) (:percentage 50)) "aaaaabbbbb")
     (:space-between 10 ((:percentage 50) (:percentage 100)) "aaaaabbbbb")
     (:space-between 10 ((:percentage 100) (:percentage 0)) "aaaaaaaaaa")
     (:space-between 10 ((:percentage 100) (:percentage 50)) "aaaaabbbbb")
     (:space-between 10 ((:percentage 100) (:percentage 100)) "aaaaabbbbb")
     (:space-between 10 ((:percentage 100) (:percentage 200)) "aaaaabbbbb"))))

(deftest letters-ratio-start
  ;; ratatui: layout::tests::split::ratio_start
  (check-letters
   '((:start 10 ((:ratio 0 1) (:ratio 0 1)) "          ")
     (:start 10 ((:ratio 0 1) (:ratio 1 4)) "bbb       ")
     (:start 10 ((:ratio 0 1) (:ratio 1 2)) "bbbbb     ")
     (:start 10 ((:ratio 0 1) (:ratio 1 1)) "bbbbbbbbbb")
     (:start 10 ((:ratio 0 1) (:ratio 2 1)) "bbbbbbbbbb")
     (:start 10 ((:ratio 1 10) (:ratio 0 1)) "a         ")
     (:start 10 ((:ratio 1 10) (:ratio 1 4)) "abbb      ")
     (:start 10 ((:ratio 1 10) (:ratio 1 2)) "abbbbb    ")
     (:start 10 ((:ratio 1 10) (:ratio 1 1)) "abbbbbbbbb")
     (:start 10 ((:ratio 1 10) (:ratio 2 1)) "abbbbbbbbb")
     (:start 10 ((:ratio 1 4) (:ratio 0 1)) "aaa       ")
     (:start 10 ((:ratio 1 4) (:ratio 1 4)) "aaabb     ")
     (:start 10 ((:ratio 1 4) (:ratio 1 2)) "aaabbbbb  ")
     (:start 10 ((:ratio 1 4) (:ratio 1 1)) "aaabbbbbbb")
     (:start 10 ((:ratio 1 4) (:ratio 2 1)) "aaabbbbbbb")
     (:start 10 ((:ratio 1 3) (:ratio 0 1)) "aaa       ")
     (:start 10 ((:ratio 1 3) (:ratio 1 4)) "aaabbb    ")
     (:start 10 ((:ratio 1 3) (:ratio 1 2)) "aaabbbbb  ")
     (:start 10 ((:ratio 1 3) (:ratio 1 1)) "aaabbbbbbb")
     (:start 10 ((:ratio 1 3) (:ratio 2 1)) "aaabbbbbbb")
     (:start 10 ((:ratio 1 2) (:ratio 0 1)) "aaaaa     ")
     (:start 10 ((:ratio 1 2) (:ratio 1 2)) "aaaaabbbbb")
     (:start 10 ((:ratio 1 2) (:ratio 1 1)) "aaaaabbbbb")
     (:start 10 ((:ratio 1 1) (:ratio 0 1)) "aaaaaaaaaa")
     (:start 10 ((:ratio 1 1) (:ratio 1 2)) "aaaaabbbbb")
     (:start 10 ((:ratio 1 1) (:ratio 1 1)) "aaaaabbbbb")
     (:start 10 ((:ratio 1 1) (:ratio 2 1)) "aaaaabbbbb"))))

(deftest letters-ratio-space-between
  ;; ratatui: layout::tests::split::ratio_spacebetween
  (check-letters
   '((:space-between 10 ((:ratio 0 1) (:ratio 0 1)) "          ")
     (:space-between 10 ((:ratio 0 1) (:ratio 1 4)) "        bb")
     (:space-between 10 ((:ratio 0 1) (:ratio 1 2)) "     bbbbb")
     (:space-between 10 ((:ratio 0 1) (:ratio 1 1)) "bbbbbbbbbb")
     (:space-between 10 ((:ratio 0 1) (:ratio 2 1)) "bbbbbbbbbb")
     (:space-between 10 ((:ratio 1 10) (:ratio 0 1)) "a         ")
     (:space-between 10 ((:ratio 1 10) (:ratio 1 4)) "a       bb")
     (:space-between 10 ((:ratio 1 10) (:ratio 1 2)) "a    bbbbb")
     (:space-between 10 ((:ratio 1 10) (:ratio 1 1)) "abbbbbbbbb")
     (:space-between 10 ((:ratio 1 10) (:ratio 2 1)) "abbbbbbbbb")
     (:space-between 10 ((:ratio 1 4) (:ratio 0 1)) "aaa       ")
     (:space-between 10 ((:ratio 1 4) (:ratio 1 4)) "aaa     bb")
     (:space-between 10 ((:ratio 1 4) (:ratio 1 2)) "aaa  bbbbb")
     (:space-between 10 ((:ratio 1 4) (:ratio 1 1)) "aaabbbbbbb")
     (:space-between 10 ((:ratio 1 4) (:ratio 2 1)) "aaabbbbbbb")
     (:space-between 10 ((:ratio 1 3) (:ratio 0 1)) "aaa       ")
     (:space-between 10 ((:ratio 1 3) (:ratio 1 4)) "aaa     bb")
     (:space-between 10 ((:ratio 1 3) (:ratio 1 2)) "aaa  bbbbb")
     (:space-between 10 ((:ratio 1 3) (:ratio 1 1)) "aaabbbbbbb")
     (:space-between 10 ((:ratio 1 3) (:ratio 2 1)) "aaabbbbbbb")
     (:space-between 10 ((:ratio 1 2) (:ratio 0 1)) "aaaaa     ")
     (:space-between 10 ((:ratio 1 2) (:ratio 1 2)) "aaaaabbbbb")
     (:space-between 10 ((:ratio 1 2) (:ratio 1 1)) "aaaaabbbbb")
     (:space-between 10 ((:ratio 1 1) (:ratio 0 1)) "aaaaaaaaaa")
     (:space-between 10 ((:ratio 1 1) (:ratio 1 2)) "aaaaabbbbb")
     (:space-between 10 ((:ratio 1 1) (:ratio 1 1)) "aaaaabbbbb")
     (:space-between 10 ((:ratio 1 1) (:ratio 2 1)) "aaaaabbbbb"))))

(deftest flex-constraint
  ;; ratatui: layout::tests::split::flex_constraint
  (check-spans
   '((:legacy 0 100 ((:length 50)) ((0 100)))
     (:start 0 100 ((:length 50)) ((0 50)))
     (:end 0 100 ((:length 50)) ((50 50)))
     (:center 0 100 ((:length 50)) ((25 50)))
     (:legacy 0 100 ((:ratio 1 2)) ((0 100)))
     (:start 0 100 ((:ratio 1 2)) ((0 50)))
     (:end 0 100 ((:ratio 1 2)) ((50 50)))
     (:center 0 100 ((:ratio 1 2)) ((25 50)))
     (:legacy 0 100 ((:percentage 50)) ((0 100)))
     (:start 0 100 ((:percentage 50)) ((0 50)))
     (:end 0 100 ((:percentage 50)) ((50 50)))
     (:center 0 100 ((:percentage 50)) ((25 50)))
     (:legacy 0 100 ((:min 50)) ((0 100)))
     (:start 0 100 ((:min 50)) ((0 100)))
     (:end 0 100 ((:min 50)) ((0 100)))
     (:center 0 100 ((:min 50)) ((0 100)))
     (:legacy 0 100 ((:max 50)) ((0 100)))
     (:start 0 100 ((:max 50)) ((0 50)))
     (:end 0 100 ((:max 50)) ((50 50)))
     (:center 0 100 ((:max 50)) ((25 50)))
     (:space-between 0 100 ((:min 1)) ((0 100)))
     (:space-between 0 100 ((:max 20)) ((0 100)))
     (:space-between 0 100 ((:length 20)) ((0 100)))
     (:legacy 0 100 ((:length 25) (:length 25)) ((0 25) (25 75)))
     (:start 0 100 ((:length 25) (:length 25)) ((0 25) (25 25)))
     (:center 0 100 ((:length 25) (:length 25)) ((25 25) (50 25)))
     (:end 0 100 ((:length 25) (:length 25)) ((50 25) (75 25)))
     (:space-between 0 100 ((:length 25) (:length 25)) ((0 25) (75 25)))
     (:space-evenly 0 100 ((:length 25) (:length 25)) ((17 25) (58 25)))
     (:space-around 0 100 ((:length 25) (:length 25)) ((13 25) (63 25)))
     (:legacy 0 100 ((:percentage 25) (:percentage 25)) ((0 25) (25 75)))
     (:start 0 100 ((:percentage 25) (:percentage 25)) ((0 25) (25 25)))
     (:center 0 100 ((:percentage 25) (:percentage 25)) ((25 25) (50 25)))
     (:end 0 100 ((:percentage 25) (:percentage 25)) ((50 25) (75 25)))
     (:space-between 0 100 ((:percentage 25) (:percentage 25)) ((0 25) (75 25)))
     (:space-evenly 0 100 ((:percentage 25) (:percentage 25)) ((17 25) (58 25)))
     (:space-around 0 100 ((:percentage 25) (:percentage 25)) ((13 25) (63 25)))
     (:legacy 0 100 ((:min 25) (:min 25)) ((0 25) (25 75)))
     (:start 0 100 ((:min 25) (:min 25)) ((0 50) (50 50)))
     (:center 0 100 ((:min 25) (:min 25)) ((0 50) (50 50)))
     (:end 0 100 ((:min 25) (:min 25)) ((0 50) (50 50)))
     (:space-between 0 100 ((:min 25) (:min 25)) ((0 50) (50 50)))
     (:space-evenly 0 100 ((:min 25) (:min 25)) ((0 50) (50 50)))
     (:space-around 0 100 ((:min 25) (:min 25)) ((0 50) (50 50)))
     (:legacy 0 100 ((:max 25) (:max 25)) ((0 25) (25 75)))
     (:start 0 100 ((:max 25) (:max 25)) ((0 25) (25 25)))
     (:center 0 100 ((:max 25) (:max 25)) ((25 25) (50 25)))
     (:end 0 100 ((:max 25) (:max 25)) ((50 25) (75 25)))
     (:space-between 0 100 ((:max 25) (:max 25)) ((0 25) (75 25)))
     (:space-evenly 0 100 ((:max 25) (:max 25)) ((17 25) (58 25)))
     (:space-around 0 100 ((:max 25) (:max 25)) ((13 25) (63 25)))
     (:space-between 0 100 ((:length 25) (:length 25) (:length 25)) ((0 25) (38 25) (75 25)))
     (:legacy 0 100 ((:length 50)) ((0 100)))
     (:start 0 100 ((:length 50)) ((0 50)))
     (:end 0 100 ((:length 50)) ((50 50)))
     (:center 0 100 ((:length 50)) ((25 50)))
     (:space-between 0 100 ((:length 50)) ((0 100)))
     (:space-evenly 0 100 ((:length 50)) ((25 50)))
     (:space-around 0 100 ((:length 50)) ((25 50))))))

(deftest flex-overlap
  ;; ratatui: layout::tests::split::flex_overlap
  (check-spans
   '((:start 0 100 ((:length 20) (:length 20) (:length 20)) ((0 20) (20 20) (40 20)))
     (:start -1 100 ((:length 20) (:length 20) (:length 20)) ((0 20) (19 20) (38 20)))
     (:center -1 100 ((:length 20) (:length 20) (:length 20)) ((21 20) (40 20) (59 20)))
     (:end -1 100 ((:length 20) (:length 20) (:length 20)) ((42 20) (61 20) (80 20)))
     (:legacy -1 100 ((:length 20) (:length 20) (:length 20)) ((0 20) (19 20) (38 62)))
     (:space-between -1 100 ((:length 20) (:length 20) (:length 20)) ((0 20) (40 20) (80 20)))
     (:space-evenly -1 100 ((:length 20) (:length 20) (:length 20)) ((10 20) (40 20) (70 20)))
     (:space-around -1 100 ((:length 20) (:length 20) (:length 20)) ((7 20) (40 20) (73 20))))))

(deftest flex-spacing
  ;; ratatui: layout::tests::split::flex_spacing
  (check-spans
   '((:start 0 100 ((:length 20) (:length 20) (:length 20)) ((0 20) (20 20) (40 20)))
     (:start 2 100 ((:length 20) (:length 20) (:length 20)) ((0 20) (22 20) (44 20)))
     (:center 2 100 ((:length 20) (:length 20) (:length 20)) ((18 20) (40 20) (62 20)))
     (:end 2 100 ((:length 20) (:length 20) (:length 20)) ((36 20) (58 20) (80 20)))
     (:legacy 2 100 ((:length 20) (:length 20) (:length 20)) ((0 20) (22 20) (44 56)))
     (:space-between 2 100 ((:length 20) (:length 20) (:length 20)) ((0 20) (40 20) (80 20)))
     (:space-evenly 2 100 ((:length 20) (:length 20) (:length 20)) ((10 20) (40 20) (70 20)))
     (:space-around 2 100 ((:length 20) (:length 20) (:length 20)) ((7 20) (40 20) (73 20))))))

(deftest constraint-priority-with-spacing
  ;; ratatui: layout::tests::split::constraint_specification_tests_for_priority_with_spacing
  (check-spans
   '((:start 0 100 ((:length 20) (:length 20) (:length 20)) ((0 20) (20 20) (40 20)))
     (:center 2 100 ((:length 20) (:length 20) (:length 20)) ((18 20) (40 20) (62 20)))
     (:end 2 100 ((:length 20) (:length 20) (:length 20)) ((36 20) (58 20) (80 20)))
     (:legacy 2 100 ((:length 20) (:length 20) (:length 20)) ((0 20) (22 20) (44 56)))
     (:legacy 2 100 ((:length 20) (:length 20) (:length 20)) ((0 20) (22 20) (44 56)))
     (:space-evenly 2 100 ((:length 20) (:length 20) (:length 20)) ((10 20) (40 20) (70 20)))
     (:space-around 2 100 ((:length 20) (:length 20) (:length 20)) ((7 20) (40 20) (73 20))))))

(deftest fill-vs-flex
  ;; ratatui: layout::tests::split::fill_vs_flex
  (check-spans
   '((:legacy 0 100 ((:length 10) (:fill 1) (:length 10)) ((0 10) (10 80) (90 10)))
     (:space-between 0 100 ((:length 10) (:length 10)) ((0 10) (90 10)))
     (:legacy 0 100 ((:fill 1) (:length 10) (:fill 1) (:length 10) (:fill 1))
      ((0 27) (27 10) (37 26) (63 10) (73 27)))
     (:space-evenly 0 100 ((:length 10) (:length 10)) ((27 10) (63 10)))
     (:legacy 0 100 ((:length 10) (:length 10) (:fill 1)) ((0 10) (10 10) (20 80)))
     (:start 0 100 ((:length 10) (:length 10)) ((0 10) (10 10)))
     (:legacy 0 100 ((:fill 1) (:length 10) (:length 10)) ((0 80) (80 10) (90 10)))
     (:end 0 100 ((:length 10) (:length 10)) ((80 10) (90 10)))
     (:legacy 0 100 ((:fill 1) (:length 10) (:length 10) (:fill 1))
      ((0 40) (40 10) (50 10) (60 40)))
     (:center 0 100 ((:length 10) (:length 10)) ((40 10) (50 10)))
     (:space-around 0 100 ((:length 10) (:length 10)) ((20 10) (70 10))))))

(deftest fill-spacing
  ;; ratatui: layout::tests::split::fill_spacing
  (check-spans
   '((:legacy 0 100 ((:fill 1) (:fill 1)) ((0 50) (50 50)))
     (:space-evenly 0 100 ((:fill 1) (:fill 1)) ((0 50) (50 50)))
     (:space-between 0 100 ((:fill 1) (:fill 1)) ((0 50) (50 50)))
     (:space-around 0 100 ((:fill 1) (:fill 1)) ((0 50) (50 50)))
     (:start 0 100 ((:fill 1) (:fill 1)) ((0 50) (50 50)))
     (:center 0 100 ((:fill 1) (:fill 1)) ((0 50) (50 50)))
     (:end 0 100 ((:fill 1) (:fill 1)) ((0 50) (50 50)))
     (:legacy 10 100 ((:fill 1) (:fill 1)) ((0 45) (55 45)))
     (:start 10 100 ((:fill 1) (:fill 1)) ((0 45) (55 45)))
     (:center 10 100 ((:fill 1) (:fill 1)) ((0 45) (55 45)))
     (:end 10 100 ((:fill 1) (:fill 1)) ((0 45) (55 45)))
     (:space-evenly 10 100 ((:fill 1) (:fill 1)) ((10 35) (55 35)))
     (:space-between 10 100 ((:fill 1) (:fill 1)) ((0 45) (55 45)))
     (:space-around 10 100 ((:fill 1) (:fill 1)) ((10 30) (60 30)))
     (:legacy 0 100 ((:fill 1) (:length 10) (:fill 1)) ((0 45) (45 10) (55 45)))
     (:space-evenly 0 100 ((:fill 1) (:length 10) (:fill 1)) ((0 45) (45 10) (55 45)))
     (:space-between 0 100 ((:fill 1) (:length 10) (:fill 1)) ((0 45) (45 10) (55 45)))
     (:space-around 0 100 ((:fill 1) (:length 10) (:fill 1)) ((0 45) (45 10) (55 45)))
     (:start 0 100 ((:fill 1) (:length 10) (:fill 1)) ((0 45) (45 10) (55 45)))
     (:center 0 100 ((:fill 1) (:length 10) (:fill 1)) ((0 45) (45 10) (55 45)))
     (:end 0 100 ((:fill 1) (:length 10) (:fill 1)) ((0 45) (45 10) (55 45)))
     (:legacy 10 100 ((:fill 1) (:length 10) (:fill 1)) ((0 35) (45 10) (65 35)))
     (:start 10 100 ((:fill 1) (:length 10) (:fill 1)) ((0 35) (45 10) (65 35)))
     (:center 10 100 ((:fill 1) (:length 10) (:fill 1)) ((0 35) (45 10) (65 35)))
     (:end 10 100 ((:fill 1) (:length 10) (:fill 1)) ((0 35) (45 10) (65 35)))
     (:space-evenly 10 100 ((:fill 1) (:length 10) (:fill 1)) ((10 25) (45 10) (65 25)))
     (:space-between 10 100 ((:fill 1) (:length 10) (:fill 1)) ((0 35) (45 10) (65 35)))
     (:space-around 10 100 ((:fill 1) (:length 10) (:fill 1)) ((10 15) (45 10) (75 15))))))

(deftest fill-overlap
  ;; ratatui: layout::tests::split::fill_overlap
  (check-spans
   '((:legacy -10 100 ((:fill 1) (:fill 1)) ((0 55) (45 55)))
     (:space-around -10 100 ((:fill 1) (:fill 1)) ((0 50) (50 50)))
     (:space-between -10 100 ((:fill 1) (:fill 1)) ((0 55) (45 55)))
     (:start -10 100 ((:fill 1) (:fill 1)) ((0 55) (45 55)))
     (:center -10 100 ((:fill 1) (:fill 1)) ((0 55) (45 55)))
     (:end -10 100 ((:fill 1) (:fill 1)) ((0 55) (45 55)))
     (:space-evenly -10 100 ((:fill 1) (:fill 1)) ((0 50) (50 50)))
     (:legacy -1 100 ((:fill 1) (:fill 1)) ((0 51) (50 50)))
     (:start -1 100 ((:fill 1) (:fill 1)) ((0 51) (50 50)))
     (:center -1 100 ((:fill 1) (:fill 1)) ((0 51) (50 50)))
     (:end -1 100 ((:fill 1) (:fill 1)) ((0 51) (50 50)))
     (:space-around -1 100 ((:fill 1) (:fill 1)) ((0 50) (50 50)))
     (:space-between -1 100 ((:fill 1) (:fill 1)) ((0 51) (50 50)))
     (:space-evenly -1 100 ((:fill 1) (:fill 1)) ((0 50) (50 50)))
     (:legacy -10 100 ((:fill 1) (:length 10) (:fill 1)) ((0 55) (45 10) (45 55)))
     (:space-around -10 100 ((:fill 1) (:length 10) (:fill 1)) ((0 45) (45 10) (55 45)))
     (:space-between -10 100 ((:fill 1) (:length 10) (:fill 1)) ((0 55) (45 10) (45 55)))
     (:start -10 100 ((:fill 1) (:length 10) (:fill 1)) ((0 55) (45 10) (45 55)))
     (:center -10 100 ((:fill 1) (:length 10) (:fill 1)) ((0 55) (45 10) (45 55)))
     (:end -10 100 ((:fill 1) (:length 10) (:fill 1)) ((0 55) (45 10) (45 55)))
     (:space-evenly -10 100 ((:fill 1) (:length 10) (:fill 1)) ((0 45) (45 10) (55 45)))
     (:legacy -1 100 ((:fill 1) (:length 10) (:fill 1)) ((0 46) (45 10) (54 46)))
     (:start -1 100 ((:fill 1) (:length 10) (:fill 1)) ((0 46) (45 10) (54 46)))
     (:center -1 100 ((:fill 1) (:length 10) (:fill 1)) ((0 46) (45 10) (54 46)))
     (:end -1 100 ((:fill 1) (:length 10) (:fill 1)) ((0 46) (45 10) (54 46)))
     (:space-around -1 100 ((:fill 1) (:length 10) (:fill 1)) ((0 45) (45 10) (55 45)))
     (:space-between -1 100 ((:fill 1) (:length 10) (:fill 1)) ((0 46) (45 10) (54 46)))
     (:space-evenly -1 100 ((:fill 1) (:length 10) (:fill 1)) ((0 45) (45 10) (55 45))))))

(deftest flex-spacing-below-user-spacing
  ;; ratatui: layout::tests::split::flex_spacing_lower_priority_than_user_spacing
  (check-spans
   '((:center 80 100 ((:length 10) (:length 10)) ((0 10) (90 10))))))

(deftest legacy-vs-default
  ;; ratatui: layout::tests::split::legacy_vs_default
  (check-spans
   '((:legacy 0 100 ((:min 10) (:length 10)) ((0 90) (90 10)))
     (:start 0 100 ((:min 10) (:length 10)) ((0 90) (90 10)))
     (:legacy 0 100 ((:min 10) (:percentage 100)) ((0 10) (10 90)))
     (:start 0 100 ((:min 10) (:percentage 100)) ((0 10) (10 90)))
     (:legacy 0 100 ((:percentage 50) (:percentage 50)) ((0 50) (50 50)))
     (:start 0 100 ((:percentage 50) (:percentage 50)) ((0 50) (50 50))))))

(deftest spacers-without-spacing
  ;; ratatui: layout::tests::split::split_with_spacers_no_spacing
  (check-spacers
   '((:legacy 0 100 ((:length 10) (:length 10)) ((0 0) (10 0) (100 0)))
     (:space-between 0 100 ((:length 10) (:length 10)) ((0 0) (10 80) (100 0)))
     (:space-evenly 0 100 ((:length 10) (:length 10)) ((0 27) (37 26) (73 27)))
     (:space-around 0 100 ((:length 10) (:length 10)) ((0 20) (30 40) (80 20)))
     (:start 0 100 ((:length 10) (:length 10)) ((0 0) (10 0) (20 80)))
     (:center 0 100 ((:length 10) (:length 10)) ((0 40) (50 0) (60 40)))
     (:end 0 100 ((:length 10) (:length 10)) ((0 80) (90 0) (100 0))))))

(deftest spacers-with-spacing
  ;; ratatui: layout::tests::split::split_with_spacers_and_spacing
  (check-spacers
   '((:legacy 5 100 ((:length 10) (:length 10)) ((0 0) (10 5) (100 0)))
     (:space-between 5 100 ((:length 10) (:length 10)) ((0 0) (10 80) (100 0)))
     (:space-evenly 5 100 ((:length 10) (:length 10)) ((0 27) (37 26) (73 27)))
     (:space-around 5 100 ((:length 10) (:length 10)) ((0 20) (30 40) (80 20)))
     (:start 5 100 ((:length 10) (:length 10)) ((0 0) (10 5) (25 75)))
     (:center 5 100 ((:length 10) (:length 10)) ((0 38) (48 5) (63 37)))
     (:end 5 100 ((:length 10) (:length 10)) ((0 75) (85 5) (100 0))))))

(deftest spacers-with-overlap
  ;; ratatui: layout::tests::split::split_with_spacers_and_overlap
  (check-spacers
   '((:legacy -1 100 ((:length 10) (:length 10)) ((0 0) (10 0) (100 0)))
     (:space-between -1 100 ((:length 10) (:length 10)) ((0 0) (10 80) (100 0)))
     (:space-evenly -1 100 ((:length 10) (:length 10)) ((0 27) (37 26) (73 27)))
     (:space-around -1 100 ((:length 10) (:length 10)) ((0 20) (30 40) (80 20)))
     (:start -1 100 ((:length 10) (:length 10)) ((0 0) (10 0) (19 81)))
     (:center -1 100 ((:length 10) (:length 10)) ((0 41) (51 0) (60 40)))
     (:end -1 100 ((:length 10) (:length 10)) ((0 81) (91 0) (100 0))))))

(deftest spacers-with-too-much-spacing
  ;; ratatui: layout::tests::split::split_with_spacers_and_too_much_spacing
  (check-spacers
   '((:legacy 200 100 ((:length 10) (:length 10)) ((0 0) (0 100) (100 0)))
     (:space-between 200 100 ((:length 10) (:length 10)) ((0 0) (0 100) (100 0)))
     (:space-evenly 200 100 ((:length 10) (:length 10)) ((0 33) (33 34) (67 33)))
     (:space-around 200 100 ((:length 10) (:length 10)) ((0 25) (25 50) (75 25)))
     (:start 200 100 ((:length 10) (:length 10)) ((0 0) (0 100) (100 0)))
     (:center 200 100 ((:length 10) (:length 10)) ((0 0) (0 100) (100 0)))
     (:end 200 100 ((:length 10) (:length 10)) ((0 0) (0 100) (100 0))))))
