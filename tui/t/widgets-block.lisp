;;;; widgets-block.lisp — tests for the block widget, mostly ported from ratatui's.

(defpackage #:tui/tests/widgets-block
  (:use #:cl)
  (:shadowing-import-from #:tui/widgets/block #:block)
  (:import-from #:tui/widgets/block #:block-inner #:block-inner-bounds)
  (:import-from #:tui/tests/harness #:deftest #:is #:signals)
  (:import-from #:tui/tests/widgets
                #:str #:blank-buffer #:render-buffer #:render-rows #:row-fgs #:row-bgs #:repeat)
  (:import-from #:tui/layout #:make-rect #:rect=)
  (:import-from #:tui/style #:make-style #:+reset+ #:+red+ #:+green+ #:+yellow+ #:+blue+)
  (:import-from #:tui/text #:span #:make-line)
  (:import-from #:tui/buffer #:buffer-row-strings #:cell-fg #:cell-bg)
  (:import-from #:tui/widgets/widget #:render))

(in-package #:tui/tests/widgets-block)

(defun line (content alignment)
  (make-line content :alignment alignment))

;;; The inner area

(deftest inner-takes-borders-into-account
  (loop for (borders area expected)
          in `((:none (0 0 0 0) (0 0 0 0))
               (:none (0 0 1 1) (0 0 1 1))
               ((:left) (0 0 0 0) (0 0 0 0))
               ((:left) (0 0 0 1) (0 0 0 1))
               ((:left) (0 0 1 1) (1 0 0 1))
               ((:left) (0 0 2 1) (1 0 1 1))
               ((:top) (0 0 1 0) (0 0 1 0))
               ((:top) (0 0 1 1) (0 1 1 0))
               ((:top) (0 0 1 2) (0 1 1 1))
               ((:right) (0 0 0 1) (0 0 0 1))
               ((:right) (0 0 1 1) (0 0 0 1))
               ((:right) (0 0 2 1) (0 0 1 1))
               ((:bottom) (0 0 1 0) (0 0 1 0))
               ((:bottom) (0 0 1 1) (0 0 1 0))
               ((:bottom) (0 0 1 2) (0 0 1 1))
               (:all (0 0 0 0) (0 0 0 0))
               (:all (0 0 1 1) (1 1 0 0))
               (:all (0 0 2 2) (1 1 0 0))
               (:all (0 0 3 3) (1 1 1 1)))
        do (is (rect= (apply #'make-rect expected)
                      (block-inner (block :borders borders) (apply #'make-rect area))))))

(deftest inner-takes-titles-into-account
  (dolist (alignment '(:left :center :right))
    (is (rect= (make-rect 0 1 0 0)
               (block-inner (block :title (line "Test" alignment)) (make-rect 0 0 0 1)))))
  (loop for (title borders expected)
          in '(((:top "Test") (:top) (0 1 0 1))
               ((:top "Test") (:bottom) (0 1 0 0))
               ((:bottom "Test") (:top) (0 1 0 0))
               ((:bottom "Test") (:bottom) (0 0 0 1)))
        do (is (rect= (apply #'make-rect expected)
                      (block-inner (block :title title :borders borders)
                                   (make-rect 0 0 0 2))))))

(deftest inner-takes-padding-into-account
  (is (rect= (make-rect 3 2 5 3)
             (block-inner (block :borders :all :padding '(2 1 1 1)) (make-rect 0 0 10 7))))
  (is (rect= (make-rect 2 2 6 3)
             (block-inner (block :borders :all :padding 1) (make-rect 0 0 10 7))))
  (is (rect= (make-rect 65535 0 0 1)
             (block-inner (block :padding '(65535 1 0 0)) (make-rect 0 0 1 1))))
  (is (equal '(11 21 8 6)
             (multiple-value-list (block-inner-bounds (block :borders :all) 10 20 10 8)))))

(deftest bad-arguments-signal-errors
  (signals type-error (block :borders :sideways))
  (signals type-error (block :borders '(:top :middle)))
  (signals type-error (block :border-type :dotted))
  (signals type-error (block :padding '(1 2)))
  (signals type-error (block :padding -1))
  (signals type-error (block :style :red))
  (signals type-error (block :title-position :middle)))

;;; Borders

(deftest border-types
  (loop for (type rows)
          in '((:plain ("┌────────┐" "│        │" "└────────┘"))
               (:rounded ("╭────────╮" "│        │" "╰────────╯"))
               (:double ("╔════════╗" "║        ║" "╚════════╝"))
               (:thick ("┏━━━━━━━━┓" "┃        ┃" "┗━━━━━━━━┛")))
        do (is (equal rows (render-rows (block :borders :all :border-type type) 10 3)))))

(deftest partial-borders
  (loop for (borders rows)
          in '(((:top :left :right :bottom) ("┌────────┐" "│        │" "└────────┘"))
               ((:top :left) ("┌─────────" "│         " "│         "))
               ((:top :right) ("─────────┐" "         │" "         │"))
               ((:bottom :left) ("│         " "│         " "└─────────"))
               ((:bottom :right) ("         │" "         │" "─────────┘"))
               ((:top :bottom) ("──────────" "          " "──────────"))
               ((:left :right) ("│        │" "│        │" "│        │"))
               (:none ("          " "          " "          ")))
        do (is (equal rows (render-rows (block :borders borders) 10 3)))))

(deftest borders-in-tiny-areas
  (is (equal '("┌") (render-rows (block :borders :all :title "I'm too big" :padding 10) 1 1)))
  (is (equal '("┌┐" "└┘") (render-rows (block :borders :all) 2 2)))
  (is (equal '() (render-rows (block :borders :all :title "x") 0 0)))
  (is (equal '("   " " ┌┐" " └┘")
             (render-rows (block :borders :all) 3 3 :area (make-rect 1 1 5 5)))))

(deftest border-style-patches-borders-and-titles
  (let ((buffer (render-buffer (block :borders :all :title "test"
                                      :border-style (make-style :fg :yellow)
                                      :style (make-style :bg :blue))
                               10 3)))
    (is (equal '("┌test────┐" "│        │" "└────────┘") (buffer-row-strings buffer)))
    (is (equal (repeat 10 +yellow+) (row-fgs buffer 0)))
    (is (equal (repeat 1 +yellow+ +reset+ +reset+ +reset+ +reset+ +reset+ +reset+ +reset+
                       +reset+ +yellow+)
               (row-fgs buffer 1)))
    (is (equal (repeat 10 +blue+) (row-bgs buffer 1)))))

;;; Titles

(deftest titles-at-top-and-bottom
  (is (equal '("┌A───B───C┐" "│         │" "└D───E───F┘")
             (render-rows (block :borders :all
                                 :titles (list (line "A" :left) (line "B" :center)
                                               (line "C" :right)
                                               (list :bottom (line "D" :left))
                                               (list :bottom (line "E" :center))
                                               (list :bottom (line "F" :right))))
                          11 3)))
  (is (equal '("    " "test") (render-rows (block :title-position :bottom :title "test") 4 2))))

(deftest title-alignment
  (loop for (alignment row) in '((:left "test    ") (:center "  test  ") (:right "    test"))
        do (is (equal (list row) (render-rows (block :title-alignment alignment :title "test")
                                              8 1))))
  ;; A line's own alignment wins over the block's.
  (loop for (block-alignment alignment row) in '((:right :left "test    ")
                                                 (:left :center "  test  ")
                                                 (:center :right "    test"))
        do (is (equal (list row)
                      (render-rows (block :title-alignment block-alignment
                                          :title (line "test" alignment))
                                   8 1)))))

(deftest several-titles-share-the-row
  (flet ((titles (&rest titles)
           (first (render-rows (block :titles titles) 10 1))))
    (is (equal "L12 L34   " (titles "L12" "L34")))
    (is (equal "L12345 L67" (titles "L12345" "L67890")))
    (is (equal " C12 C34  " (titles (line "C12" :center) (line "C34" :center))))
    (is (equal "12345 C678" (titles (line "C12345" :center) (line "C67890" :center))))
    (is (equal "   R12 R34" (titles (line "R12" :right) (line "R34" :right))))
    (is (equal "345 R67890" (titles (line "R12345" :right) (line "R67890" :right))))
    (is (equal "L1C5678   " (titles "L1234" (line "C5678" :center))))
    (is (equal "L123R67890" (titles "L12345" (line "R67890" :right))))
    (is (equal "  C1R67890" (titles (line "C12345" :center) (line "R67890" :right))))))

(deftest empty-right-aligned-title-draws-nothing
  (is (equal (repeat 3 "               ")
             (render-rows (block :title-alignment :right :title "") 15 3))))

(deftest title-styles
  (dolist (alignment '(:left :center :right))
    ;; The span's style, the block's title style, and the span's style over it.
    (let ((buffer (render-buffer (block :title-alignment alignment
                                        :title (span "test" (make-style :fg :yellow)))
                                 4 1)))
      (is (equal (repeat 4 +yellow+) (row-fgs buffer 0))))
    (let ((buffer (render-buffer (block :title-alignment alignment :title "test"
                                        :title-style (make-style :fg :yellow))
                                 4 1)))
      (is (equal (repeat 4 +yellow+) (row-fgs buffer 0))))
    (let ((buffer (render-buffer (block :title-alignment alignment
                                        :title (span "test" (make-style :fg :yellow))
                                        :title-style (make-style :fg :green :bg :red))
                                 4 1)))
      (is (equal (repeat 4 +yellow+) (row-fgs buffer 0)))
      (is (equal (repeat 4 +red+) (row-bgs buffer 0))))))

(deftest titles-with-wide-characters
  (is (equal (list (str "┌" #x65E5 #x672C "─┐"))
             (render-rows (block :borders :all :title (str #x65E5 #x672C)) 7 1)))
  ;; A wide character that doesn't fit is left out.
  (is (equal (list (str "┌" #x65E5 "─┐"))
             (render-rows (block :borders :all :title (str #x65E5 #x672C)) 5 1))))

(deftest block-style-patches-the-area
  (let ((buffer (blank-buffer 4 2)))
    (render (block :style (make-style :bg :green)) (make-rect 1 0 2 2) buffer)
    (is (equal (list +reset+ +green+ +green+ +reset+) (row-bgs buffer 1)))))
