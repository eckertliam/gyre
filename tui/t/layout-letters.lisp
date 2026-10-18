;;;; layout-letters.lisp — ratatui's layout tables for each constraint kind under :legacy flex,
;;;; shown as letters: in "aab" the first area is two cells wide and the second one.

(defpackage #:tui/tests/layout-letters
  (:use #:cl)
  (:import-from #:tui/tests/harness #:deftest)
  (:import-from #:tui/tests/layout #:check-letters))

(in-package #:tui/tests/layout-letters)

(deftest letters-length
  ;; ratatui: layout::tests::split::length
  (check-letters
   '((:legacy 1 ((:length 0)) "a")  ; zero
     (:legacy 1 ((:length 1)) "a")  ; exact
     (:legacy 1 ((:length 2)) "a")  ; overflow
     (:legacy 2 ((:length 0)) "aa")  ; zero
     (:legacy 2 ((:length 1)) "aa")  ; underflow
     (:legacy 2 ((:length 2)) "aa")  ; exact
     (:legacy 2 ((:length 3)) "aa")  ; overflow
     (:legacy 1 ((:length 0) (:length 0)) "b")  ; zero, zero
     (:legacy 1 ((:length 0) (:length 1)) "b")  ; zero, exact
     (:legacy 1 ((:length 0) (:length 2)) "b")  ; zero, overflow
     (:legacy 1 ((:length 1) (:length 0)) "a")  ; exact, zero
     (:legacy 1 ((:length 1) (:length 1)) "a")  ; exact, exact
     (:legacy 1 ((:length 1) (:length 2)) "a")  ; exact, overflow
     (:legacy 1 ((:length 2) (:length 0)) "a")  ; overflow, zero
     (:legacy 1 ((:length 2) (:length 1)) "a")  ; overflow, exact
     (:legacy 1 ((:length 2) (:length 2)) "a")  ; overflow, overflow
     (:legacy 2 ((:length 0) (:length 0)) "bb")  ; zero, zero
     (:legacy 2 ((:length 0) (:length 1)) "bb")  ; zero, underflow
     (:legacy 2 ((:length 0) (:length 2)) "bb")  ; zero, exact
     (:legacy 2 ((:length 0) (:length 3)) "bb")  ; zero, overflow
     (:legacy 2 ((:length 1) (:length 0)) "ab")  ; underflow, zero
     (:legacy 2 ((:length 1) (:length 1)) "ab")  ; underflow, underflow
     (:legacy 2 ((:length 1) (:length 2)) "ab")  ; underflow, exact
     (:legacy 2 ((:length 1) (:length 3)) "ab")  ; underflow, overflow
     (:legacy 2 ((:length 2) (:length 0)) "aa")  ; exact, zero
     (:legacy 2 ((:length 2) (:length 1)) "aa")  ; exact, underflow
     (:legacy 2 ((:length 2) (:length 2)) "aa")  ; exact, exact
     (:legacy 2 ((:length 2) (:length 3)) "aa")  ; exact, overflow
     (:legacy 2 ((:length 3) (:length 0)) "aa")  ; overflow, zero
     (:legacy 2 ((:length 3) (:length 1)) "aa")  ; overflow, underflow
     (:legacy 2 ((:length 3) (:length 2)) "aa")  ; overflow, exact
     (:legacy 2 ((:length 3) (:length 3)) "aa")  ; overflow, overflow
     (:legacy 3 ((:length 2) (:length 2)) "aab"))))  ; with stretchlast

(deftest letters-max
  ;; ratatui: layout::tests::split::max
  (check-letters
   '((:legacy 1 ((:max 0)) "a")  ; zero
     (:legacy 1 ((:max 1)) "a")  ; exact
     (:legacy 1 ((:max 2)) "a")  ; overflow
     (:legacy 2 ((:max 0)) "aa")  ; zero
     (:legacy 2 ((:max 1)) "aa")  ; underflow
     (:legacy 2 ((:max 2)) "aa")  ; exact
     (:legacy 2 ((:max 3)) "aa")  ; overflow
     (:legacy 1 ((:max 0) (:max 0)) "b")  ; zero, zero
     (:legacy 1 ((:max 0) (:max 1)) "b")  ; zero, exact
     (:legacy 1 ((:max 0) (:max 2)) "b")  ; zero, overflow
     (:legacy 1 ((:max 1) (:max 0)) "a")  ; exact, zero
     (:legacy 1 ((:max 1) (:max 1)) "a")  ; exact, exact
     (:legacy 1 ((:max 1) (:max 2)) "a")  ; exact, overflow
     (:legacy 1 ((:max 2) (:max 0)) "a")  ; overflow, zero
     (:legacy 1 ((:max 2) (:max 1)) "a")  ; overflow, exact
     (:legacy 1 ((:max 2) (:max 2)) "a")  ; overflow, overflow
     (:legacy 2 ((:max 0) (:max 0)) "bb")  ; zero, zero
     (:legacy 2 ((:max 0) (:max 1)) "bb")  ; zero, underflow
     (:legacy 2 ((:max 0) (:max 2)) "bb")  ; zero, exact
     (:legacy 2 ((:max 0) (:max 3)) "bb")  ; zero, overflow
     (:legacy 2 ((:max 1) (:max 0)) "ab")  ; underflow, zero
     (:legacy 2 ((:max 1) (:max 1)) "ab")  ; underflow, underflow
     (:legacy 2 ((:max 1) (:max 2)) "ab")  ; underflow, exact
     (:legacy 2 ((:max 1) (:max 3)) "ab")  ; underflow, overflow
     (:legacy 2 ((:max 2) (:max 0)) "aa")  ; exact, zero
     (:legacy 2 ((:max 2) (:max 1)) "aa")  ; exact, underflow
     (:legacy 2 ((:max 2) (:max 2)) "aa")  ; exact, exact
     (:legacy 2 ((:max 2) (:max 3)) "aa")  ; exact, overflow
     (:legacy 2 ((:max 3) (:max 0)) "aa")  ; overflow, zero
     (:legacy 2 ((:max 3) (:max 1)) "aa")  ; overflow, underflow
     (:legacy 2 ((:max 3) (:max 2)) "aa")  ; overflow, exact
     (:legacy 2 ((:max 3) (:max 3)) "aa")  ; overflow, overflow
     (:legacy 3 ((:max 2) (:max 2)) "aab"))))

(deftest letters-min
  ;; ratatui: layout::tests::split::min
  (check-letters
   '((:legacy 1 ((:min 0) (:min 0)) "b")  ; zero, zero
     (:legacy 1 ((:min 0) (:min 1)) "b")  ; zero, exact
     (:legacy 1 ((:min 0) (:min 2)) "b")  ; zero, overflow
     (:legacy 1 ((:min 1) (:min 0)) "a")  ; exact, zero
     (:legacy 1 ((:min 1) (:min 1)) "a")  ; exact, exact
     (:legacy 1 ((:min 1) (:min 2)) "a")  ; exact, overflow
     (:legacy 1 ((:min 2) (:min 0)) "a")  ; overflow, zero
     (:legacy 1 ((:min 2) (:min 1)) "a")  ; overflow, exact
     (:legacy 1 ((:min 2) (:min 2)) "a")  ; overflow, overflow
     (:legacy 2 ((:min 0) (:min 0)) "bb")  ; zero, zero
     (:legacy 2 ((:min 0) (:min 1)) "bb")  ; zero, underflow
     (:legacy 2 ((:min 0) (:min 2)) "bb")  ; zero, exact
     (:legacy 2 ((:min 0) (:min 3)) "bb")  ; zero, overflow
     (:legacy 2 ((:min 1) (:min 0)) "ab")  ; underflow, zero
     (:legacy 2 ((:min 1) (:min 1)) "ab")  ; underflow, underflow
     (:legacy 2 ((:min 1) (:min 2)) "ab")  ; underflow, exact
     (:legacy 2 ((:min 1) (:min 3)) "ab")  ; underflow, overflow
     (:legacy 2 ((:min 2) (:min 0)) "aa")  ; exact, zero
     (:legacy 2 ((:min 2) (:min 1)) "aa")  ; exact, underflow
     (:legacy 2 ((:min 2) (:min 2)) "aa")  ; exact, exact
     (:legacy 2 ((:min 2) (:min 3)) "aa")  ; exact, overflow
     (:legacy 2 ((:min 3) (:min 0)) "aa")  ; overflow, zero
     (:legacy 2 ((:min 3) (:min 1)) "aa")  ; overflow, underflow
     (:legacy 2 ((:min 3) (:min 2)) "aa")  ; overflow, exact
     (:legacy 2 ((:min 3) (:min 3)) "aa")  ; overflow, overflow
     (:legacy 3 ((:min 2) (:min 2)) "aab"))))

(deftest letters-percentage
  ;; ratatui: layout::tests::split::percentage
  (check-letters
   '((:legacy 1 ((:percentage 0)) "a")
     (:legacy 1 ((:percentage 25)) "a")
     (:legacy 1 ((:percentage 50)) "a")
     (:legacy 1 ((:percentage 90)) "a")
     (:legacy 1 ((:percentage 100)) "a")
     (:legacy 1 ((:percentage 200)) "a")
     (:legacy 2 ((:percentage 0)) "aa")
     (:legacy 2 ((:percentage 10)) "aa")
     (:legacy 2 ((:percentage 25)) "aa")
     (:legacy 2 ((:percentage 50)) "aa")
     (:legacy 2 ((:percentage 66)) "aa")
     (:legacy 2 ((:percentage 100)) "aa")
     (:legacy 2 ((:percentage 200)) "aa")
     (:legacy 10 ((:percentage 0)) "aaaaaaaaaa")
     (:legacy 10 ((:percentage 10)) "aaaaaaaaaa")
     (:legacy 10 ((:percentage 25)) "aaaaaaaaaa")
     (:legacy 10 ((:percentage 50)) "aaaaaaaaaa")
     (:legacy 10 ((:percentage 66)) "aaaaaaaaaa")
     (:legacy 10 ((:percentage 100)) "aaaaaaaaaa")
     (:legacy 10 ((:percentage 200)) "aaaaaaaaaa")
     (:legacy 1 ((:percentage 0) (:percentage 0)) "b")
     (:legacy 1 ((:percentage 0) (:percentage 10)) "b")
     (:legacy 1 ((:percentage 0) (:percentage 50)) "b")
     (:legacy 1 ((:percentage 0) (:percentage 90)) "b")
     (:legacy 1 ((:percentage 0) (:percentage 100)) "b")
     (:legacy 1 ((:percentage 0) (:percentage 200)) "b")
     (:legacy 1 ((:percentage 10) (:percentage 0)) "b")
     (:legacy 1 ((:percentage 10) (:percentage 10)) "b")
     (:legacy 1 ((:percentage 10) (:percentage 50)) "b")
     (:legacy 1 ((:percentage 10) (:percentage 90)) "b")
     (:legacy 1 ((:percentage 10) (:percentage 100)) "b")
     (:legacy 1 ((:percentage 10) (:percentage 200)) "b")
     (:legacy 1 ((:percentage 50) (:percentage 0)) "a")
     (:legacy 1 ((:percentage 50) (:percentage 50)) "a")
     (:legacy 1 ((:percentage 50) (:percentage 100)) "a")
     (:legacy 1 ((:percentage 50) (:percentage 200)) "a")
     (:legacy 1 ((:percentage 90) (:percentage 0)) "a")
     (:legacy 1 ((:percentage 90) (:percentage 50)) "a")
     (:legacy 1 ((:percentage 90) (:percentage 100)) "a")
     (:legacy 1 ((:percentage 90) (:percentage 200)) "a")
     (:legacy 1 ((:percentage 100) (:percentage 0)) "a")
     (:legacy 1 ((:percentage 100) (:percentage 50)) "a")
     (:legacy 1 ((:percentage 100) (:percentage 100)) "a")
     (:legacy 1 ((:percentage 100) (:percentage 200)) "a")
     (:legacy 2 ((:percentage 0) (:percentage 0)) "bb")
     (:legacy 2 ((:percentage 0) (:percentage 25)) "bb")
     (:legacy 2 ((:percentage 0) (:percentage 50)) "bb")
     (:legacy 2 ((:percentage 0) (:percentage 100)) "bb")
     (:legacy 2 ((:percentage 0) (:percentage 200)) "bb")
     (:legacy 2 ((:percentage 10) (:percentage 0)) "bb")
     (:legacy 2 ((:percentage 10) (:percentage 25)) "bb")
     (:legacy 2 ((:percentage 10) (:percentage 50)) "bb")
     (:legacy 2 ((:percentage 10) (:percentage 100)) "bb")
     (:legacy 2 ((:percentage 10) (:percentage 200)) "bb")
     (:legacy 2 ((:percentage 25) (:percentage 0)) "ab")
     (:legacy 2 ((:percentage 25) (:percentage 25)) "ab")
     (:legacy 2 ((:percentage 25) (:percentage 50)) "ab")
     (:legacy 2 ((:percentage 25) (:percentage 100)) "ab")
     (:legacy 2 ((:percentage 25) (:percentage 200)) "ab")
     (:legacy 2 ((:percentage 33) (:percentage 0)) "ab")
     (:legacy 2 ((:percentage 33) (:percentage 25)) "ab")
     (:legacy 2 ((:percentage 33) (:percentage 50)) "ab")
     (:legacy 2 ((:percentage 33) (:percentage 100)) "ab")
     (:legacy 2 ((:percentage 33) (:percentage 200)) "ab")
     (:legacy 2 ((:percentage 50) (:percentage 0)) "ab")
     (:legacy 2 ((:percentage 50) (:percentage 50)) "ab")
     (:legacy 2 ((:percentage 50) (:percentage 100)) "ab")
     (:legacy 2 ((:percentage 100) (:percentage 0)) "aa")
     (:legacy 2 ((:percentage 100) (:percentage 50)) "aa")
     (:legacy 2 ((:percentage 100) (:percentage 100)) "aa")
     (:legacy 3 ((:percentage 33) (:percentage 33)) "abb")
     (:legacy 3 ((:percentage 33) (:percentage 66)) "abb")
     (:legacy 4 ((:percentage 33) (:percentage 33)) "abbb")
     (:legacy 4 ((:percentage 33) (:percentage 66)) "abbb")
     (:legacy 10 ((:percentage 0) (:percentage 0)) "bbbbbbbbbb")
     (:legacy 10 ((:percentage 0) (:percentage 25)) "bbbbbbbbbb")
     (:legacy 10 ((:percentage 0) (:percentage 50)) "bbbbbbbbbb")
     (:legacy 10 ((:percentage 0) (:percentage 100)) "bbbbbbbbbb")
     (:legacy 10 ((:percentage 0) (:percentage 200)) "bbbbbbbbbb")
     (:legacy 10 ((:percentage 10) (:percentage 0)) "abbbbbbbbb")
     (:legacy 10 ((:percentage 10) (:percentage 25)) "abbbbbbbbb")
     (:legacy 10 ((:percentage 10) (:percentage 50)) "abbbbbbbbb")
     (:legacy 10 ((:percentage 10) (:percentage 100)) "abbbbbbbbb")
     (:legacy 10 ((:percentage 10) (:percentage 200)) "abbbbbbbbb")
     (:legacy 10 ((:percentage 25) (:percentage 0)) "aaabbbbbbb")
     (:legacy 10 ((:percentage 25) (:percentage 25)) "aaabbbbbbb")
     (:legacy 10 ((:percentage 25) (:percentage 50)) "aaabbbbbbb")
     (:legacy 10 ((:percentage 25) (:percentage 100)) "aaabbbbbbb")
     (:legacy 10 ((:percentage 25) (:percentage 200)) "aaabbbbbbb")
     (:legacy 10 ((:percentage 33) (:percentage 0)) "aaabbbbbbb")
     (:legacy 10 ((:percentage 33) (:percentage 25)) "aaabbbbbbb")
     (:legacy 10 ((:percentage 33) (:percentage 50)) "aaabbbbbbb")
     (:legacy 10 ((:percentage 33) (:percentage 100)) "aaabbbbbbb")
     (:legacy 10 ((:percentage 33) (:percentage 200)) "aaabbbbbbb")
     (:legacy 10 ((:percentage 50) (:percentage 0)) "aaaaabbbbb")
     (:legacy 10 ((:percentage 50) (:percentage 50)) "aaaaabbbbb")
     (:legacy 10 ((:percentage 50) (:percentage 100)) "aaaaabbbbb")
     (:legacy 10 ((:percentage 100) (:percentage 0)) "aaaaaaaaaa")
     (:legacy 10 ((:percentage 100) (:percentage 50)) "aaaaaaaaaa")
     (:legacy 10 ((:percentage 100) (:percentage 100)) "aaaaaaaaaa"))))

(deftest letters-ratio
  ;; ratatui: layout::tests::split::ratio
  (check-letters
   '((:legacy 1 ((:ratio 0 1)) "a")
     (:legacy 1 ((:ratio 1 4)) "a")
     (:legacy 1 ((:ratio 1 2)) "a")
     (:legacy 1 ((:ratio 9 10)) "a")
     (:legacy 1 ((:ratio 1 1)) "a")
     (:legacy 1 ((:ratio 2 1)) "a")
     (:legacy 2 ((:ratio 0 1)) "aa")
     (:legacy 2 ((:ratio 1 10)) "aa")
     (:legacy 2 ((:ratio 1 4)) "aa")
     (:legacy 2 ((:ratio 1 2)) "aa")
     (:legacy 2 ((:ratio 2 3)) "aa")
     (:legacy 2 ((:ratio 1 1)) "aa")
     (:legacy 2 ((:ratio 2 1)) "aa")
     (:legacy 1 ((:ratio 0 1) (:ratio 0 1)) "b")
     (:legacy 1 ((:ratio 0 1) (:ratio 1 10)) "b")
     (:legacy 1 ((:ratio 0 1) (:ratio 1 2)) "b")
     (:legacy 1 ((:ratio 0 1) (:ratio 9 10)) "b")
     (:legacy 1 ((:ratio 0 1) (:ratio 1 1)) "b")
     (:legacy 1 ((:ratio 0 1) (:ratio 2 1)) "b")
     (:legacy 1 ((:ratio 1 10) (:ratio 0 1)) "b")
     (:legacy 1 ((:ratio 1 10) (:ratio 1 10)) "b")
     (:legacy 1 ((:ratio 1 10) (:ratio 1 2)) "b")
     (:legacy 1 ((:ratio 1 10) (:ratio 9 10)) "b")
     (:legacy 1 ((:ratio 1 10) (:ratio 1 1)) "b")
     (:legacy 1 ((:ratio 1 10) (:ratio 2 1)) "b")
     (:legacy 1 ((:ratio 1 2) (:ratio 0 1)) "a")
     (:legacy 1 ((:ratio 1 2) (:ratio 1 2)) "a")
     (:legacy 1 ((:ratio 1 2) (:ratio 1 1)) "a")
     (:legacy 1 ((:ratio 1 2) (:ratio 2 1)) "a")
     (:legacy 1 ((:ratio 9 10) (:ratio 0 1)) "a")
     (:legacy 1 ((:ratio 9 10) (:ratio 1 2)) "a")
     (:legacy 1 ((:ratio 9 10) (:ratio 1 1)) "a")
     (:legacy 1 ((:ratio 9 10) (:ratio 2 1)) "a")
     (:legacy 1 ((:ratio 1 1) (:ratio 0 1)) "a")
     (:legacy 1 ((:ratio 1 1) (:ratio 1 2)) "a")
     (:legacy 1 ((:ratio 1 1) (:ratio 1 1)) "a")
     (:legacy 1 ((:ratio 1 1) (:ratio 2 1)) "a")
     (:legacy 2 ((:ratio 0 1) (:ratio 0 1)) "bb")
     (:legacy 2 ((:ratio 0 1) (:ratio 1 4)) "bb")
     (:legacy 2 ((:ratio 0 1) (:ratio 1 2)) "bb")
     (:legacy 2 ((:ratio 0 1) (:ratio 1 1)) "bb")
     (:legacy 2 ((:ratio 0 1) (:ratio 2 1)) "bb")
     (:legacy 2 ((:ratio 1 10) (:ratio 0 1)) "bb")
     (:legacy 2 ((:ratio 1 10) (:ratio 1 4)) "bb")
     (:legacy 2 ((:ratio 1 10) (:ratio 1 2)) "bb")
     (:legacy 2 ((:ratio 1 10) (:ratio 1 1)) "bb")
     (:legacy 2 ((:ratio 1 10) (:ratio 2 1)) "bb")
     (:legacy 2 ((:ratio 1 4) (:ratio 0 1)) "ab")
     (:legacy 2 ((:ratio 1 4) (:ratio 1 4)) "ab")
     (:legacy 2 ((:ratio 1 4) (:ratio 1 2)) "ab")
     (:legacy 2 ((:ratio 1 4) (:ratio 1 1)) "ab")
     (:legacy 2 ((:ratio 1 4) (:ratio 2 1)) "ab")
     (:legacy 2 ((:ratio 1 3) (:ratio 0 1)) "ab")
     (:legacy 2 ((:ratio 1 3) (:ratio 1 4)) "ab")
     (:legacy 2 ((:ratio 1 3) (:ratio 1 2)) "ab")
     (:legacy 2 ((:ratio 1 3) (:ratio 1 1)) "ab")
     (:legacy 2 ((:ratio 1 3) (:ratio 2 1)) "ab")
     (:legacy 2 ((:ratio 1 2) (:ratio 0 1)) "ab")
     (:legacy 2 ((:ratio 1 2) (:ratio 1 2)) "ab")
     (:legacy 2 ((:ratio 1 2) (:ratio 1 1)) "ab")
     (:legacy 2 ((:ratio 1 1) (:ratio 0 1)) "aa")
     (:legacy 2 ((:ratio 1 1) (:ratio 1 2)) "aa")
     (:legacy 2 ((:ratio 1 1) (:ratio 1 1)) "aa")
     (:legacy 3 ((:ratio 1 3) (:ratio 1 3)) "abb")
     (:legacy 3 ((:ratio 1 3) (:ratio 2 3)) "abb")
     (:legacy 10 ((:ratio 0 1) (:ratio 0 1)) "bbbbbbbbbb")
     (:legacy 10 ((:ratio 0 1) (:ratio 1 4)) "bbbbbbbbbb")
     (:legacy 10 ((:ratio 0 1) (:ratio 1 2)) "bbbbbbbbbb")
     (:legacy 10 ((:ratio 0 1) (:ratio 1 1)) "bbbbbbbbbb")
     (:legacy 10 ((:ratio 0 1) (:ratio 2 1)) "bbbbbbbbbb")
     (:legacy 10 ((:ratio 1 10) (:ratio 0 1)) "abbbbbbbbb")
     (:legacy 10 ((:ratio 1 10) (:ratio 1 4)) "abbbbbbbbb")
     (:legacy 10 ((:ratio 1 10) (:ratio 1 2)) "abbbbbbbbb")
     (:legacy 10 ((:ratio 1 10) (:ratio 1 1)) "abbbbbbbbb")
     (:legacy 10 ((:ratio 1 10) (:ratio 2 1)) "abbbbbbbbb")
     (:legacy 10 ((:ratio 1 4) (:ratio 0 1)) "aaabbbbbbb")
     (:legacy 10 ((:ratio 1 4) (:ratio 1 4)) "aaabbbbbbb")
     (:legacy 10 ((:ratio 1 4) (:ratio 1 2)) "aaabbbbbbb")
     (:legacy 10 ((:ratio 1 4) (:ratio 1 1)) "aaabbbbbbb")
     (:legacy 10 ((:ratio 1 4) (:ratio 2 1)) "aaabbbbbbb")
     (:legacy 10 ((:ratio 1 3) (:ratio 0 1)) "aaabbbbbbb")
     (:legacy 10 ((:ratio 1 3) (:ratio 1 4)) "aaabbbbbbb")
     (:legacy 10 ((:ratio 1 3) (:ratio 1 2)) "aaabbbbbbb")
     (:legacy 10 ((:ratio 1 3) (:ratio 1 1)) "aaabbbbbbb")
     (:legacy 10 ((:ratio 1 3) (:ratio 2 1)) "aaabbbbbbb")
     (:legacy 10 ((:ratio 1 2) (:ratio 0 1)) "aaaaabbbbb")
     (:legacy 10 ((:ratio 1 2) (:ratio 1 2)) "aaaaabbbbb")
     (:legacy 10 ((:ratio 1 2) (:ratio 1 1)) "aaaaabbbbb")
     (:legacy 10 ((:ratio 1 1) (:ratio 0 1)) "aaaaaaaaaa")
     (:legacy 10 ((:ratio 1 1) (:ratio 1 2)) "aaaaaaaaaa")
     (:legacy 10 ((:ratio 1 1) (:ratio 1 1)) "aaaaaaaaaa"))))
