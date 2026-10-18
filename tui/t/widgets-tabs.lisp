;;;; widgets-tabs.lisp — tests for the tabs and scrollbar widgets, mostly ported from ratatui's.

(defpackage #:tui/tests/widgets-tabs
  (:use #:cl)
  (:shadowing-import-from #:tui/widgets/block #:block)
  (:import-from #:tui/widgets/tabs #:tabs)
  (:import-from #:tui/widgets/scrollbar
                #:scrollbar #:make-scrollbar-state #:scrollbar-state-position)
  (:import-from #:tui/tests/harness #:deftest #:is #:signals)
  (:import-from #:tui/tests/widgets
                #:str #:blank-buffer #:filled-buffer #:render-buffer #:render-rows #:row-fgs
                #:row-modifiers #:repeat)
  (:import-from #:tui/layout #:make-rect)
  (:import-from #:tui/style #:make-style #:+reset+ #:+red+ #:+blue+ #:+reverse+ #:+underline+)
  (:import-from #:tui/text #:span)
  (:import-from #:tui/buffer #:buffer-row-strings)
  (:import-from #:tui/widgets/widget #:render #:render-stateful))

(in-package #:tui/tests/widgets-tabs)

;;; Tabs

(defun four-tabs (&rest options)
  (apply #'tabs '("Tab1" "Tab2" "Tab3" "Tab4") options))

(defun highlighted (buffer y)
  "The columns of row Y of BUFFER drawn reversed, as a list of (start end) runs."
  (let ((runs '())
        (start nil))
    (loop for modifiers in (append (row-modifiers buffer y) '(0))
          for x from 0
          do (cond ((and (logtest modifiers +reverse+) (null start)) (setf start x))
                   ((and (not (logtest modifiers +reverse+)) start)
                    (push (list start x) runs)
                    (setf start nil))))
    (nreverse runs)))

(deftest tabs-render
  (let ((buffer (render-buffer (four-tabs) 30 1)))
    (is (equal (list (str " Tab1 " #x2502 " Tab2 " #x2502 " Tab3 " #x2502 " Tab4    "))
               (buffer-row-strings buffer)))
    (is (equal '((1 5)) (highlighted buffer 0)))))

(deftest tabs-padding
  (let ((buffer (render-buffer (four-tabs :padding-left "" :padding-right "") 30 1)))
    (is (equal (list (str "Tab1" #x2502 "Tab2" #x2502 "Tab3" #x2502 "Tab4           "))
               (buffer-row-strings buffer)))
    (is (equal '((0 4)) (highlighted buffer 0))))
  (let ((buffer (render-buffer (four-tabs :padding-left "---") 40 1)))
    (is (equal (list (str "---Tab1 " #x2502 "---Tab2 " #x2502 "---Tab3 " #x2502
                          "---Tab4      "))
               (buffer-row-strings buffer)))
    (is (equal '((3 7)) (highlighted buffer 0))))
  (is (equal (list (str " Tab1++" #x2502 " Tab2++" #x2502 " Tab3++" #x2502 " Tab4++         "))
             (render-rows (four-tabs :padding-right "++") 40 1)))
  (is (equal (list (str "---Tab1++" #x2502 "---Tab2++" #x2502 "---Tab3++" #x2502))
             (render-rows (four-tabs :padding-left "---" :padding-right "++") 30 1))))

(deftest tabs-with-block
  (let ((buffer (render-buffer (four-tabs :block (block :borders :all :title "Tabs")) 30 3)))
    (is (equal (list "┌Tabs────────────────────────┐"
                     (str "│ Tab1 " #x2502 " Tab2 " #x2502 " Tab3 " #x2502 " Tab4  │")
                     "└────────────────────────────┘")
               (buffer-row-strings buffer)))
    (is (equal '((2 6)) (highlighted buffer 1)))))

(deftest tabs-style-and-selection
  (let ((buffer (render-buffer (four-tabs :style (make-style :fg :red)) 30 1)))
    (is (equal (repeat 30 +red+) (row-fgs buffer 0))))
  (loop for (selected runs) in '((0 ((1 5))) (1 ((8 12))) (3 ((22 26))) (4 ()) (nil ()))
        do (is (equal runs (highlighted (render-buffer (four-tabs :selected selected) 30 1) 0))))
  (let ((buffer (render-buffer (four-tabs :style (make-style :fg :red)
                                          :highlight-style (make-style :modifiers :underline))
                               30 1)))
    (is (equal (repeat 30 +red+) (row-fgs buffer 0)))
    (is (equal (repeat 1 0 +underline+ +underline+ +underline+ +underline+ 0)
               (subseq (row-modifiers buffer 0) 0 6)))))

(deftest tabs-divider
  (is (equal '(" Tab1 -- Tab2 -- Tab3 -- Tab4 ") (render-rows (four-tabs :divider "--") 30 1)))
  (let ((buffer (render-buffer (four-tabs :divider (span "|" (make-style :fg :blue))) 30 1)))
    (is (= +blue+ (nth 6 (row-fgs buffer 0))))))

(deftest tabs-in-small-areas
  (is (equal '(" ") (render-rows (four-tabs :selected 1 :divider "|") 1 1)))
  (is (equal '() (render-rows (four-tabs) 0 0)))
  (is (equal '(" Ta") (render-rows (four-tabs) 3 1)))
  (is (equal '("     ") (render-rows (tabs '()) 5 1))))

(deftest tabs-with-wide-characters
  ;; A wide character that doesn't fit at the right edge is left out.
  (let ((buffer (render-buffer (tabs (list (str #x65E5 #x672C) "b")) 4 1)))
    (is (equal (list (str " " #x65E5 " ")) (buffer-row-strings buffer)))
    (is (equal '((1 3)) (highlighted buffer 0)))))

(deftest tabs-bad-arguments
  (signals type-error (tabs '("a") :selected -1))
  (signals type-error (tabs '("a") :divider 3)))

;;; Scrollbar

(defun bar (&rest options)
  "A horizontal-top scrollbar without arrows, with a - track and a # thumb, as ratatui's
tests use."
  ;; OPTIONS come first: the first of duplicate keyword arguments wins.
  (apply #'scrollbar (append options '(:orientation :horizontal-top :begin-symbol nil
                                       :end-symbol nil :track-symbol "-" :thumb-symbol "#"))))

(defun bar-row (scrollbar width content-length position &key (viewport-length 0))
  (first (render-rows scrollbar width 1
                      :state (make-scrollbar-state :content-length content-length
                                                   :position position
                                                   :viewport-length viewport-length))))

(deftest scrollbar-simple
  (is (equal "#-" (bar-row (bar) 2 2 0)))
  (is (equal "-#" (bar-row (bar) 2 2 1)))
  (loop for (position expected) in '((0 "#####-----") (1 "-#####----") (2 "-#####----")
                                     (3 "--#####---") (4 "--#####---") (5 "---#####--")
                                     (6 "---#####--") (7 "----#####-") (8 "----#####-")
                                     (9 "-----#####"))
        do (is (equal expected (bar-row (bar) 10 10 position)))))

(deftest scrollbar-full-and-empty
  (is (equal "          " (bar-row (bar) 10 0 0)))
  (is (equal "##########" (bar-row (bar) 10 1 0)))
  (is (equal "#########-" (bar-row (bar) 10 2 0)))
  (is (equal "-#########" (bar-row (bar) 10 2 1))))

(deftest scrollbar-default-symbols
  (let ((track (string (code-char #x2550)))
        (thumb (string (code-char #x2588))))
    (flet ((expected (before thumbs after)
             (apply #'str (append (repeat before track) (repeat thumbs thumb)
                                  (repeat after track)))))
      (loop for (position before) in '((0 0) (1 1) (2 1) (3 2) (4 2) (5 3) (6 3) (7 4) (8 4)
                                       (9 5) (100 5))
            do (is (equal (expected before 5 (- 5 before))
                          (bar-row (scrollbar :orientation :horizontal-bottom
                                              :begin-symbol nil :end-symbol nil)
                                   10 10 position)))))))

(deftest scrollbar-without-track
  (let ((thumb (string (code-char #x2588))))
    (is (equal (str " " thumb thumb thumb thumb thumb "    ")
               (bar-row (scrollbar :orientation :horizontal-bottom :track-symbol nil
                                   :begin-symbol nil :end-symbol nil)
                        10 10 1)))
    ;; The cells of the track are left as they were.
    (let ((buffer (filled-buffer 10 1 #\-)))
      (render-stateful (scrollbar :orientation :horizontal-bottom :track-symbol nil
                                  :begin-symbol nil :end-symbol nil)
                       (make-rect 0 0 10 1) buffer
                       (make-scrollbar-state :content-length 10 :position 9))
      (is (equal (list (str "-----" thumb thumb thumb thumb thumb)) (buffer-row-strings buffer))))))

(deftest scrollbar-with-arrows
  (loop for (position expected) in '((0 "<####---->") (1 "<####---->") (2 "<-####--->")
                                     (3 "<-####--->") (4 "<--####-->") (5 "<--####-->")
                                     (6 "<---####->") (7 "<---####->") (8 "<---####->")
                                     (9 "<----####>") (10 "<----####>") (500 "<----####>"))
        do (is (equal expected (bar-row (bar :begin-symbol "<" :end-symbol ">") 10 10 position)))
           ;; Vertical bars, left and right, draw the same symbols top to bottom.
           (dolist (orientation '(:vertical-left :vertical-right))
             (let ((rows (render-rows (scrollbar :orientation orientation :begin-symbol "<"
                                                 :end-symbol ">" :track-symbol "-"
                                                 :thumb-symbol "#")
                                      5 10
                                      :state (make-scrollbar-state :content-length 10
                                                                   :position position))))
               (is (equal (map 'list (lambda (c)
                                       (if (eq orientation :vertical-left)
                                           (str c "    ")
                                           (str "    " c)))
                               expected)
                          rows))))))

(deftest scrollbar-horizontal-edges
  (let ((state (make-scrollbar-state :content-length 10 :position 0)))
    (is (equal '("          " "#####-----")
               (render-rows (bar :orientation :horizontal-bottom) 10 2 :state state)))
    (is (equal '("#####-----" "          ")
               (render-rows (bar :orientation :horizontal-top) 10 2 :state state)))))

(deftest scrollbar-viewport-length
  (is (equal "--------##" (bar-row (bar) 10 10 10 :viewport-length 2)))
  (is (equal "----#" (bar-row (bar) 5 100 100 :viewport-length 2)))
  (is (equal "<-----#################>"
             (bar-row (bar :begin-symbol "<" :end-symbol ">") 24 9 8))))

(deftest scrollbar-thumb-arithmetic
  ;; The parts always add up to the track, the thumb is at least one cell and stays inside.
  (let ((scrollbar (scrollbar)))
    (dotimes (content 30)
      (dotimes (position 32)
        (dolist (viewport '(0 1 5 40))
          (multiple-value-bind (before thumb after)
              (tui/widgets/scrollbar::part-lengths
               scrollbar (make-scrollbar-state :content-length content :position position
                                               :viewport-length viewport)
               1 12)
            (is (= 10 (+ before thumb after)))
            (is (<= 1 thumb 10))
            (is (<= 0 before))
            (is (<= 0 after))))))
    (is (equal '(0 0 0) (multiple-value-list
                         (tui/widgets/scrollbar::part-lengths
                          scrollbar (make-scrollbar-state :content-length 10 :position 5) 1 2))))
    (is (equal '(0 0 0) (multiple-value-list
                         (tui/widgets/scrollbar::part-lengths
                          (bar) (make-scrollbar-state :content-length 10 :position 3) 0 1))))))

(deftest scrollbar-in-small-or-outside-areas
  (dolist (orientation '(:vertical-left :vertical-right :horizontal-top :horizontal-bottom))
    (is (equal '(" ") (render-rows (scrollbar :orientation orientation) 1 1
                                   :state (make-scrollbar-state :content-length 10 :position 5))))
    (is (equal '() (render-rows (scrollbar :orientation orientation) 0 0
                                :state (make-scrollbar-state :content-length 10 :position 5))))
    (dolist (area (list (make-rect 0 10 10 8) (make-rect 40 10 10 8) (make-rect 10 10 0 8)))
      (let ((buffer (blank-buffer 30 8 10 10))
            (state (make-scrollbar-state :content-length 10 :position 3)))
        (render-stateful (scrollbar :orientation orientation) area buffer state)
        (is (equal (repeat 8 (make-string 30 :initial-element #\Space))
                   (buffer-row-strings buffer)))
        (is (= 3 (scrollbar-state-position state)))))))

(deftest scrollbar-styles
  (let ((buffer (render-buffer (bar :begin-symbol "<" :end-symbol ">"
                                    :style (make-style :fg :red)
                                    :thumb-style (make-style :fg :blue))
                               6 1 :state (make-scrollbar-state :content-length 4
                                                                :viewport-length 1))))
    (is (equal '("<#--->") (buffer-row-strings buffer)))
    (is (equal (list +red+ +blue+ +red+ +red+ +red+ +red+) (row-fgs buffer 0))))
  (signals type-error (scrollbar :orientation :diagonal))
  (signals error (render (scrollbar) (make-rect 0 0 1 1) (blank-buffer 1 1))))
