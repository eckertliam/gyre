;;;; widgets-table.lisp — tests for the table widget and its state, mostly ported from
;;;; ratatui's.

(defpackage #:tui/tests/widgets-table
  (:use #:cl)
  (:shadowing-import-from #:tui/widgets/block #:block)
  (:import-from #:tui/widgets/table #:table)
  (:import-from #:tui/widgets/table-row #:table-row #:table-row-cells #:table-row-height)
  (:import-from #:tui/widgets/table-state
                #:make-table-state #:table-state-selected #:table-state-offset #:table-select
                #:table-select-next #:table-select-previous #:table-select-first
                #:table-select-last)
  (:import-from #:tui/tests/harness #:deftest #:is #:signals)
  (:import-from #:tui/tests/widgets
                #:str #:blank-buffer #:render-buffer #:render-rows #:row-fgs #:row-bgs #:repeat)
  (:import-from #:tui/layout #:make-rect #:rect-x #:rect-width)
  (:import-from #:tui/style #:make-style #:+reset+ #:+red+ #:+blue+ #:+green+)
  (:import-from #:tui/text #:make-line #:make-text #:text-height)
  (:import-from #:tui/buffer #:buffer-row-strings)
  (:import-from #:tui/widgets/widget #:render #:render-stateful))

(in-package #:tui/tests/widgets-table)

(defun state (&key selected (offset 0))
  (make-table-state :selected selected :offset offset))

(defun two-rows ()
  '(("Cell1" "Cell2") ("Cell3" "Cell4")))

(defun columns (table max-width selection-width)
  "The columns' (x width) when TABLE is MAX-WIDTH wide with SELECTION-WIDTH for the symbol."
  (map 'list (lambda (rect) (list (rect-x rect) (rect-width rect)))
       (tui/widgets/table::column-areas table max-width selection-width)))

;;; State

(deftest table-state-selection
  (let ((s (state)))
    (table-select-next s)
    (is (eql 0 (table-state-selected s)))
    (table-select-next s)
    (table-select-previous s)
    (table-select-previous s)
    (is (eql 0 (table-state-selected s)))
    (table-select-last s)
    (is (eql most-positive-fixnum (table-state-selected s)))
    (table-select-first s)
    (is (eql 0 (table-state-selected s)))
    (setf (table-state-offset s) 3)
    (table-select s nil)
    (is (null (table-state-selected s)))
    (is (eql 0 (table-state-offset s)))
    (table-select-previous s)
    (is (eql most-positive-fixnum (table-state-selected s)))))

(deftest rendering-clamps-the-selection
  (let ((s (state :selected 0)))
    (render-stateful (table '()) (make-rect 0 0 10 3) (blank-buffer 10 3) s)
    (is (null (table-state-selected s))))
  (let ((s (state)))
    (table-select-last s)
    (render-stateful (table '(("a")) :widths '((:length 1))) (make-rect 0 0 10 3)
                     (blank-buffer 10 3) s)
    (is (eql 0 (table-state-selected s)))))

;;; Rows

(deftest rows-from-designators
  (let ((row (table-row '("a" "b") :height 2)))
    (is (= 2 (length (table-row-cells row))))
    (is (= 2 (table-row-height row))))
  (is (= 2 (text-height (svref (table-row-cells (table-row (list (format nil "x~%y")))) 0))))
  (signals type-error (table-row "abc"))
  (signals type-error (table-row '("a") :height -1)))

;;; Rendering

(deftest empty-areas-and-tables
  (let ((buffer (blank-buffer 15 3)))
    (render (table (two-rows) :widths '((:length 5) (:length 5))) (make-rect 0 0 0 0) buffer)
    (render (table '()) (make-rect 0 0 15 3) buffer)
    (is (equal (repeat 3 "               ") (buffer-row-strings buffer))))
  (is (equal '("┌") (render-rows (table (two-rows) :block (block :borders :all)) 1 1)))
  (is (equal '() (render-rows (table (two-rows)) 0 0))))

(deftest with-block
  (is (equal '("┌Block────────┐" "│Cell1 Cell2  │" "└─────────────┘")
             (render-rows (table (two-rows) :widths '((:length 5) (:length 5))
                                            :block (block :borders :all :title "Block"))
                          15 3))))

(deftest header-and-footer
  (let ((widths '((:length 5) (:length 5))))
    (is (equal '("Head1 Head2    " "Cell1 Cell2    " "Cell3 Cell4    ")
               (render-rows (table (two-rows) :widths widths :header '("Head1" "Head2")) 15 3)))
    (is (equal '("Cell1 Cell2    " "Cell3 Cell4    " "Foot1 Foot2    ")
               (render-rows (table (two-rows) :widths widths :footer '("Foot1" "Foot2")) 15 3)))
    (is (equal '("Head1 Head2    " "Cell1 Cell2    " "Foot1 Foot2    ")
               (render-rows (table '(("Cell1" "Cell2")) :widths widths
                                   :header '("Head1" "Head2") :footer '("Foot1" "Foot2"))
                            15 3)))
    (is (equal '("Head1 Head2    " "               " "Cell1 Cell2    ")
               (render-rows (table (two-rows) :widths widths
                                   :header (table-row '("Head1" "Head2") :bottom-margin 1))
                            15 3)))
    (is (equal '("Cell1 Cell2    " "               " "Foot1 Foot2    ")
               (render-rows (table '(("Cell1" "Cell2")) :widths widths
                                   :footer (table-row '("Foot1" "Foot2") :top-margin 1))
                            15 3)))))

(deftest header-and-footer-styles
  (let ((buffer (render-buffer (table (two-rows) :widths '((:length 5) (:length 5))
                                      :header (table-row '("H1" "H2")
                                                         :style (make-style :fg :red))
                                      :footer (table-row '("F1" "F2")
                                                         :style (make-style :fg :blue)))
                               15 4)))
    (is (equal '("H1    H2       " "Cell1 Cell2    " "Cell3 Cell4    " "F1    F2       ")
               (buffer-row-strings buffer)))
    (is (equal (repeat 15 +red+) (row-fgs buffer 0)))
    (is (equal (repeat 15 +reset+) (row-fgs buffer 1)))
    (is (equal (repeat 15 +blue+) (row-fgs buffer 3)))))

(deftest margins-and-tall-rows
  (is (equal '("Cell1 Cell2    " "               " "Cell3 Cell4    ")
             (render-rows (table (list (table-row '("Cell1" "Cell2") :bottom-margin 1)
                                       '("Cell3" "Cell4"))
                                 :widths '((:length 5) (:length 5)))
                          15 3)))
  (is (equal '("Cell1       Cell2      " "Cell3-Line1 Cell4-Line1" "Cell3-Line2 Cell4-Line2")
             (render-rows (table (list '("Cell1" "Cell2")
                                       (table-row
                                        (list (format nil "Cell3-Line1~%Cell3-Line2~%Cell3-Line3")
                                              (format nil "Cell4-Line1~%Cell4-Line2~%Cell4-Line3"))
                                        :height 3))
                                 :widths '((:length 11) (:length 11)))
                          23 3))))

(deftest cell-alignment
  (is (equal '("Left      " "  Center  " "     Right")
             (render-rows (table (list (list (make-line "Left" :alignment :left))
                                       (list (make-line "Center" :alignment :center))
                                       (list (make-line "Right" :alignment :right)))
                                 :widths '((:percentage 100)))
                          10 3))))

(deftest selection
  (let ((buffer (render-buffer (table (two-rows) :widths '((:length 5) (:length 5))
                                                 :row-highlight-style (make-style :fg :red)
                                                 :highlight-symbol ">>")
                               15 3 :state (state :selected 0))))
    (is (equal '(">>Cell1 Cell2  " "  Cell3 Cell4  " "               ")
               (buffer-row-strings buffer)))
    (is (equal (repeat 15 +red+) (row-fgs buffer 0)))
    (is (equal (repeat 15 +reset+) (row-fgs buffer 1)))))

(deftest selection-and-offset
  (let ((rows (loop for i below 100 collect (list (princ-to-string i)))))
    (loop for (selected offset expected)
            in '((nil 50 ("50" "51" "52" "53" "54"))
                 (20 20 ("20" "21" "22" "23" "24"))
                 (49 49 ("49" "50" "51" "52" "53"))
                 (50 50 ("50" "51" "52" "53" "54"))
                 (54 50 ("50" "51" "52" "53" "54"))
                 (55 51 ("51" "52" "53" "54" "55"))
                 (80 76 ("76" "77" "78" "79" "80")))
          do (let ((s (state :selected selected :offset 50)))
               (is (equal expected (render-rows (table rows :widths '((:length 2))) 2 5
                                                :state s)))
               (is (eql offset (table-state-offset s)))))))

(deftest column-widths
  (flet ((widths (constraints max-width selection-width &rest options)
           (columns (apply #'table '() :widths constraints options) max-width selection-width)))
    (is (equal '((0 4) (5 4)) (widths '((:length 4) (:length 4)) 20 0)))
    (is (equal '((3 4) (8 4)) (widths '((:length 4) (:length 4)) 20 3)))
    (is (equal '((0 3) (4 3)) (widths '((:length 4) (:length 4)) 7 0)))
    (is (equal '((3 2) (6 1)) (widths '((:length 4) (:length 4)) 7 3)))
    (is (equal '((0 10) (10 10) (20 10))
               (columns (table '(("a" "b") ("c" "d" "e")) :header '("f" "g") :footer '("h" "i")
                                                          :column-spacing 0)
                        30 0)))
    (is (equal '((0 5) (5 5)) (columns (table '() :header '("f" "g") :column-spacing 0) 10 0)))
    (is (equal '((0 5) (5 5)) (columns (table '() :footer '("h" "i") :column-spacing 0) 10 0)))))

(deftest highlight-spacing-and-column-widths
  (flet ((check (spacing selected expected)
           (is (equal expected
                      (render-rows (table '(("ABCDE" "12345")) :highlight-spacing spacing
                                                               :highlight-symbol ">>>"
                                                               :column-spacing 0)
                                   15 3 :state (state :selected selected))))))
    (let ((blank "               "))
      (check :never nil (list "ABCDE  12345   " blank blank))
      (check :never 0 (list "ABCDE  12345   " blank blank))
      (check :when-selected nil (list "ABCDE  12345   " blank blank))
      (check :when-selected 0 (list ">>>ABCDE 12345 " blank blank))
      (check :always nil (list "   ABCDE 12345 " blank blank))
      (check :always 0 (list ">>>ABCDE 12345 " blank blank))))
  (is (equal '("ABCDE12345     ")
             (render-rows (table '(("ABCDE" "12345")) :widths '((:length 5) (:length 5))
                                                      :column-spacing 0)
                          15 1))))

(deftest scroll-padding
  (let ((rows (loop for i below 6 collect (list (format nil "Row ~D" i)))))
    (loop for (height offset padding selected expected-offset)
            in `((4 0 1 3 1) (4 4 1 2 1) (5 2 0 3 2) (5 2 2 3 1) (4 0 4 1 0) (4 2 1 nil 2)
                 (6 0 ,most-positive-fixnum 3 0) (4 0 ,most-positive-fixnum 3 1))
          do (let ((s (state :selected selected :offset offset)))
               (render-stateful (table rows :widths '((:length 10)) :scroll-padding padding)
                                (make-rect 0 0 10 height) (blank-buffer 10 height) s)
               (is (eql expected-offset (table-state-offset s)))
               (is (eql selected (table-state-selected s)))))
    (loop for (offset padding selected expected)
            in '((0 1 3 ("Row 1     " "Row 2     " "Row 3     " "Row 4     "))
                 (4 1 2 ("Row 1     " "Row 2     " "Row 3     " "Row 4     "))
                 (0 0 5 ("Row 2     " "Row 3     " "Row 4     " "Row 5     ")))
          do (is (equal expected
                        (render-rows (table rows :widths '((:length 10)) :scroll-padding padding)
                                     10 4 :state (state :selected selected :offset offset)))))))

(deftest scroll-padding-is-stable
  (let ((table (table (loop for i below 8 collect (list (format nil "Row ~D" i)))
                      :widths '((:length 10)) :scroll-padding 3))
        (s (state :selected 4 :offset 2))
        (buffer (blank-buffer 10 5)))
    (render-stateful table (make-rect 0 0 10 5) buffer s)
    (let ((offset (table-state-offset s)))
      (render-stateful table (make-rect 0 0 10 5) buffer s)
      (is (eql offset (table-state-offset s))))))

(deftest scroll-padding-with-tall-rows-and-margins
  (let ((s (state :selected 3)))
    (render-stateful (table (list '("Row 0") '("Row 1") '("Row 2") '("Row 3")
                                  (table-row '("Row 4") :height 3) '("Row 5"))
                            :widths '((:length 10)) :scroll-padding 1)
                     (make-rect 0 0 10 3) (blank-buffer 10 3) s)
    (is (<= (table-state-offset s) 3)))
  (let ((s (state :selected 4)))
    (render-stateful (table (append (loop for i below 5
                                          collect (table-row (list (format nil "Row ~D" i))
                                                             :bottom-margin 1))
                                    (list '("Row 5")))
                            :widths '((:length 10)) :scroll-padding 1)
                     (make-rect 0 0 10 6) (blank-buffer 10 6) s)
    (is (<= (table-state-offset s) 3))))

(deftest partly-visible-last-row
  (is (equal '("a" "b" "c")
             (render-rows (table (list '("a") (table-row (list (format nil "b~%c~%d")) :height 3))
                                 :widths '((:length 1)))
                          1 3))))

(deftest cells-beyond-the-columns-and-wide-characters
  (is (equal (list (str "ab " #x65E5 " "))
             (render-rows (table (list (list "ab" (str #x65E5 #x672C) "zz"))
                                 :widths '((:length 2) (:length 3)))
                          6 1))))

(deftest row-styles-and-highlight-order
  (let ((buffer (render-buffer (table (list (table-row '("a" "b") :style (make-style :bg :blue))
                                            '("c" "d"))
                                      :widths '((:length 1) (:length 1))
                                      :style (make-style :fg :green)
                                      :row-highlight-style (make-style :bg :red))
                               4 2 :state (state :selected 0))))
    (is (equal (repeat 4 +red+) (row-bgs buffer 0)))
    (is (equal (repeat 4 +green+) (row-fgs buffer 0)))
    (is (equal (repeat 4 +reset+) (row-bgs buffer 1)))))

(deftest highlight-symbol-stays-inside-the-area
  (is (equal '(">>  ") (render-rows (table '(("a")) :highlight-symbol ">>>>") 4 1
                                    :area (make-rect 0 0 2 1) :state (state :selected 0)))))

(deftest bad-arguments-signal-errors
  (signals error (table '(("a")) :widths '((:length -1))))
  (signals error (table '(("a")) :flex :sideways))
  (signals type-error (table '(("a")) :highlight-spacing :sometimes))
  (signals type-error (table '("abc"))))
