;;;; widgets-list.lisp — tests for the list widget and its state, mostly ported from ratatui's.

(defpackage #:tui/tests/widgets-list
  (:use #:cl)
  (:shadowing-import-from #:tui/widgets/block #:block)
  (:shadowing-import-from #:tui/widgets/list #:list)
  (:import-from #:tui/widgets/list-state
                #:make-list-state #:list-state-selected #:list-state-offset #:list-select
                #:list-select-next #:list-select-previous #:list-select-first
                #:list-select-last)
  (:import-from #:tui/tests/harness #:deftest #:is #:signals)
  (:import-from #:tui/tests/widgets
                #:str #:blank-buffer #:render-buffer #:render-rows #:row-fgs #:row-modifiers
                #:repeat)
  (:import-from #:tui/layout #:make-rect)
  (:import-from #:tui/style #:make-style #:+reset+ #:+red+ #:+yellow+ #:+bold+)
  (:import-from #:tui/text #:span #:make-line)
  (:import-from #:tui/buffer #:buffer-row-strings)
  (:import-from #:tui/widgets/widget #:render #:render-stateful))

(in-package #:tui/tests/widgets-list)

(defun items (&rest names) names)

(defun numbered (count)
  (loop for i below count collect (format nil "Item ~D" i)))

(defun state (&key selected (offset 0))
  (make-list-state :selected selected :offset offset))

;;; State

(deftest list-state-selection
  (let ((s (state)))
    (is (null (list-state-selected s)))
    (list-select-next s)
    (is (eql 0 (list-state-selected s)))
    (list-select-next s)
    (is (eql 1 (list-state-selected s)))
    (list-select-previous s)
    (list-select-previous s)
    (is (eql 0 (list-state-selected s)))
    (list-select-last s)
    (is (eql most-positive-fixnum (list-state-selected s)))
    (list-select-next s)
    (is (eql most-positive-fixnum (list-state-selected s)))
    (list-select-first s)
    (is (eql 0 (list-state-selected s)))
    (setf (list-state-offset s) 4)
    (list-select s 2)
    (is (eql 4 (list-state-offset s)))
    (list-select s nil)
    (is (null (list-state-selected s)))
    (is (eql 0 (list-state-offset s)))
    (list-select-previous s)
    (is (eql most-positive-fixnum (list-state-selected s)))
    (signals type-error (list-select s -1))))

(deftest rendering-clamps-the-selection
  (let ((s (state))
        (one (list (items "Item 1"))))
    (flet ((draw (list) (render-stateful list (make-rect 0 0 10 1) (blank-buffer 10 1) s)))
      (list-select-first s)
      (draw (list '()))
      (is (null (list-state-selected s)))
      (list-select-first s)
      (draw one)
      (is (eql 0 (list-state-selected s)))
      (list-select-last s)
      (draw one)
      (is (eql 0 (list-state-selected s)))
      (list-select-previous s)
      (draw one)
      (is (eql 0 (list-state-selected s)))
      (list-select-next s)
      (draw one)
      (is (eql 0 (list-state-selected s))))))

;;; Rendering

(deftest nothing-in-small-areas
  (let ((buffer (blank-buffer 15 3))
        (items (items "Item 0" "Item 1" "Item 2")))
    (render (list items :highlight-symbol ">>") (make-rect 0 0 0 3) buffer)
    (render (list items :highlight-symbol ">>") (make-rect 0 0 15 0) buffer)
    (is (equal (repeat 3 "               ") (buffer-row-strings buffer)))
    (render (list items :highlight-symbol ">>" :block (block :borders :all))
            (make-rect 0 0 15 2) buffer)
    (is (equal '("┌─────────────┐" "└─────────────┘" "               ")
               (buffer-row-strings buffer)))))

(deftest combinations
  (flet ((check (items selected expected)
           (let ((widget (list items :highlight-symbol ">>")))
             (is (equal expected
                        (if (eq selected :none)
                            (render-rows widget 10 5)
                            (render-rows widget 10 5 :state (state :selected selected))))))))
    (dolist (selected '(:none nil 0))
      (check '() selected (repeat 5 "          ")))
    (dolist (selected '(:none nil))
      (check '("Item 0") selected (cons "Item 0    " (repeat 4 "          ")))
      (check (numbered 3) selected (list* "Item 0    " "Item 1    " "Item 2    "
                                          (repeat 2 "          ")))
      (check (items (format nil "Item 0~%Line 2") "Item 1" "Item 2") selected
             (cl:list "Item 0    " "Line 2    " "Item 1    " "Item 2    " "          ")))
    (dolist (selected '(0 1))
      (check '("Item 0") selected (cons ">>Item 0  " (repeat 4 "          "))))
    (check (numbered 3) 0 (list* ">>Item 0  " "  Item 1  " "  Item 2  " (repeat 2 "          ")))
    (check (numbered 3) 1 (list* "  Item 0  " ">>Item 1  " "  Item 2  " (repeat 2 "          ")))
    (check (numbered 3) 3 (list* "  Item 0  " "  Item 1  " ">>Item 2  " (repeat 2 "          ")))
    (check (items (format nil "Item 0~%Line 2") "Item 1" "Item 2") 0
           (cl:list ">>Item 0  " "  Line 2  " "  Item 1  " "  Item 2  " "          "))
    (check (items (format nil "Item 0~%Line 2") "Item 1" "Item 2") 1
           (cl:list "  Item 0  " "  Line 2  " ">>Item 1  " "  Item 2  " "          "))))

(deftest empty-strings-and-block
  (is (equal '("┌List────┐" "│Item 0  │" "│        │" "│        │" "│Item 1  │" "│Item 2  │"
               "└────────┘")
             (render-rows (list (items "Item 0" "" "" "Item 1" "Item 2")
                                :block (block :borders :all :title "List"))
                          10 7)))
  (is (equal '("┌List────┐" "│Item 0  │" "│Item 1  │" "│Item 2  │" "│        │" "│        │"
               "└────────┘")
             (render-rows (list (numbered 3) :block (block :borders :all :title "List"))
                          10 7))))

(deftest list-style
  (let ((buffer (render-buffer (list (numbered 3) :style (make-style :fg :red)) 10 5)))
    (dotimes (y 5)
      (is (equal (repeat 10 +red+) (row-fgs buffer y))))))

(deftest highlight-symbol-and-style
  (let ((buffer (render-buffer (list (numbered 3) :highlight-symbol ">>"
                                     :highlight-style (make-style :fg :yellow))
                               10 5 :state (state :selected 1))))
    (is (equal (list* "  Item 0  " ">>Item 1  " "  Item 2  " (repeat 2 "          "))
               (buffer-row-strings buffer)))
    (is (equal (repeat 10 +reset+) (row-fgs buffer 0)))
    (is (equal (repeat 10 +yellow+) (row-fgs buffer 1))))
  (let* ((symbol (make-line ">>" :style (make-style :fg :red :modifiers :bold)))
         (buffer (render-buffer (list (numbered 3) :highlight-symbol symbol
                                                   :highlight-style (make-style :fg :yellow))
                                10 5 :state (state :selected 1))))
    ;; As in ratatui, the symbol is drawn after the highlight style.
    (is (equal (repeat 2 +red+ +yellow+ +yellow+ +yellow+ +yellow+ +yellow+ +yellow+ +yellow+
                       +yellow+)
               (row-fgs buffer 1)))
    (is (equal (repeat 2 +bold+ 0 0 0 0 0 0 0 0) (row-modifiers buffer 1)))))

(deftest highlight-spacing
  (flet ((check (spacing selected expected)
           (is (equal expected
                      (render-rows (list (numbered 3) :highlight-symbol ">>"
                                                      :highlight-spacing spacing)
                                   10 5 :state (state :selected selected))))))
    (let ((plain (list* "Item 0    " "Item 1    " "Item 2    " (repeat 2 "          ")))
          (spaced (list* "  Item 0  " "  Item 1  " "  Item 2  " (repeat 2 "          ")))
          (selected (list* "  Item 0  " ">>Item 1  " "  Item 2  " (repeat 2 "          "))))
      (check :when-selected nil plain)
      (check :when-selected 1 selected)
      (check :always nil spaced)
      (check :always 1 selected)
      (check :never nil plain)
      (check :never 1 plain))))

(deftest repeat-highlight-symbol
  (let ((buffer (render-buffer (list (items (format nil "Item 0~%Line 2") "Item 1" "Item 2")
                                     :highlight-symbol ">>" :repeat-highlight-symbol t
                                     :highlight-style (make-style :fg :yellow))
                               10 5 :state (state :selected 0))))
    (is (equal '(">>Item 0  " ">>Line 2  " "  Item 1  " "  Item 2  " "          ")
               (buffer-row-strings buffer)))
    (is (equal (repeat 10 +yellow+) (row-fgs buffer 1)))))

(deftest direction
  (is (equal '("Item 0    " "Item 1    " "Item 2    " "          ")
             (render-rows (list (numbered 3) :direction :top-to-bottom) 10 4)))
  (is (equal '("          " "Item 2    " "Item 1    " "Item 0    ")
             (render-rows (list (numbered 3) :direction :bottom-to-top) 10 4)))
  (is (equal '("  Item 2  " "  Item 1  " ">>Item 0  ")
             (render-rows (list (numbered 5) :direction :bottom-to-top :highlight-symbol ">>")
                          10 3 :state (state :selected 0))))
  (is (equal '(">>Item 4  " "  Item 3  " "  Item 2  ")
             (render-rows (list (numbered 5) :direction :bottom-to-top :highlight-symbol ">>")
                          10 3 :state (state :selected 4)))))

(deftest truncation-and-offset
  (is (equal '("Item 0    " "Item 1    " "Item 2    ") (render-rows (list (numbered 5)) 10 3)))
  (is (equal '("Item 3" "Item 4" "Item 5")
             (render-rows (list (numbered 7)) 6 3 :state (state :offset 3))))
  (let ((long (items "Item 0 with a very long line that will be truncated" "Item 1" "Item 2")))
    (is (equal '("Item 0 with a v" "Item 1         " "Item 2         ")
               (render-rows (list long :highlight-symbol ">>") 15 3 :state (state))))
    (is (equal '(">>Item 0 with a" "  Item 1       " "  Item 2       ")
               (render-rows (list long :highlight-symbol ">>") 15 3
                            :state (state :selected 0))))))

(deftest selection-scrolls-into-view
  (let ((s (state :selected 1 :offset 3)))
    (is (equal '(">>Item 1  " "  Item 2  " "  Item 3  ")
               (render-rows (list (numbered 7) :highlight-symbol ">>") 10 3 :state s)))
    (is (eql 1 (list-state-selected s)))
    (is (eql 1 (list-state-offset s))))
  (let ((s (state :selected 6 :offset 3)))
    (is (equal '("  Item 4  " "  Item 5  " ">>Item 6  ")
               (render-rows (list (numbered 7) :highlight-symbol ">>") 10 3 :state s)))
    (is (eql 6 (list-state-selected s)))
    (is (eql 4 (list-state-offset s)))))

(deftest alignment
  (let ((lines (items (make-line "Left" :alignment :left)
                      (make-line "Center" :alignment :center)
                      (make-line "Right" :alignment :right))))
    (is (equal '("Left      " "  Center  " "     Right" "          ")
               (render-rows (list lines) 10 4))))
  (let ((lines (items (make-line "Odd" :alignment :left) (make-line "Even" :alignment :center)
                      (make-line "Width" :alignment :right))))
    (is (equal '("Odd    " " Even  " "  Width" "       ") (render-rows (list lines) 7 4)))
    (is (equal '("Odd   " " Even " " Width" "      ") (render-rows (list lines) 6 4)))
    (is (equal '("Odd     " "  Even  " "   Width" "        ") (render-rows (list lines) 8 4))))
  (is (equal '("  Small   " "          ")
             (render-rows (list (items (make-line "Small" :alignment :center))) 10 2)))
  (is (equal '("Large" "     ")
             (render-rows (list (items (make-line "Large line" :alignment :left))) 5 2))))

(deftest scroll-padding
  (loop for (height offset padding selected expected)
          in '((4 2 0 2 (">> Item 2 " "   Item 3 " "   Item 4 " "   Item 5 "))
               (4 2 1 2 ("   Item 1 " ">> Item 2 " "   Item 3 " "   Item 4 "))
               (4 1 1 4 ("   Item 2 " "   Item 3 " ">> Item 4 " "   Item 5 "))
               (4 1 2 4 ("   Item 2 " "   Item 3 " ">> Item 4 " "   Item 5 "))
               (5 2 0 3 ("   Item 2 " ">> Item 3 " "   Item 4 " "   Item 5 " "          "))
               (5 2 2 3 ("   Item 1 " "   Item 2 " ">> Item 3 " "   Item 4 " "   Item 5 "))
               (4 0 4 1 ("   Item 0 " ">> Item 1 " "   Item 2 " "   Item 3 ")))
        do (is (equal expected
                      (render-rows (list (numbered 6) :scroll-padding padding
                                                      :highlight-symbol ">> ")
                                   10 height :state (state :selected selected :offset offset))))))

(deftest scroll-padding-is-stable
  (let ((s (state :selected 4 :offset 2))
        (widget (list (numbered 8) :scroll-padding 3 :highlight-symbol ">> "))
        (buffer (blank-buffer 10 5)))
    (render-stateful widget (make-rect 0 0 10 5) buffer s)
    (let ((offset (list-state-offset s)))
      (render-stateful widget (make-rect 0 0 10 5) buffer s)
      (is (eql offset (list-state-offset s))))))

(deftest scroll-padding-with-tall-items
  (is (equal '("   Item 1 " "   Item 2 " ">> Item 3 ")
             (render-rows (list (items "Item 0" "Item 1" "Item 2" "Item 3"
                                      (format nil "Item 4~%Test~%Test") "Item 5")
                                :scroll-padding 1 :highlight-symbol ">> ")
                          10 3 :state (state :selected 3))))
  (is (equal '("   Item 1 " ">> Item 2 " "   Item 3 " "          ")
             (render-rows (list (items (format nil "Item 0~%Test~%Test") "Item 1" "Item 2"
                                      "Item 3")
                                :scroll-padding 2 :highlight-symbol ">> ")
                          10 4 :state (state :selected 2 :offset 1)))))

(deftest items-taller-than-the-area-are-not-shown
  ;; As in ratatui: an item that can't be shown whole isn't shown at all.
  (is (equal '(" " " ")
             (render-rows (list (items (format nil "a~%b~%c"))) 1 2 :state (state :selected 0))))
  (is (equal '(" " " ")
             (render-rows (list (items (format nil "a~%b~%c")) :direction :bottom-to-top)
                          1 2 :state (state :selected 0))))
  (is (equal '("┌─┐" "│ │" "│ │" "└─┘")
             (render-rows (list (items (format nil "a~%b~%c~%d")) :block (block :borders :all))
                          3 4)))
  (is (equal '("a" "b" " ")
             (render-rows (list (items (format nil "a~%b") (format nil "c~%d"))) 1 3))))

(deftest highlight-symbol-wider-than-the-area
  (loop for (symbol expected) in '((">>>>" ">>>>Item1 ") (">>>>>" ">>>>>Item1")
                                   (">>>>>>" ">>>>>>Item"))
        do (is (equal (items expected)
                      (render-rows (list '("Item1") :highlight-symbol symbol) 10 1
                                   :state (state :selected 0)))))
  (is (equal '(">>>  ") (render-rows (list '("Item1") :highlight-symbol ">>>>>>") 5 1
                                     :area (make-rect 0 0 3 1) :state (state :selected 0)))))

(deftest wide-characters-in-items
  (is (equal (items (str ">" #x65E5 #x672C " ") (str " " #x65E5 "   "))
             (render-rows (list (items (str #x65E5 #x672C #x8A9E) (str #x65E5))
                                :highlight-symbol ">")
                          6 2 :state (state :selected 0)))))

(deftest bad-arguments-signal-errors
  (signals type-error (list '("a") :direction :left-to-right))
  (signals type-error (list '("a") :highlight-spacing :sometimes))
  (signals type-error (list '("a") :scroll-padding -1))
  (signals error (list '(42)))
  (signals error (render-stateful (list '("a")) (make-rect 0 0 1 1) (blank-buffer 1 1)
                                  (tui/widgets/table-state:make-table-state))))
