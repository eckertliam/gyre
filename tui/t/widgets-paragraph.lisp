;;;; widgets-paragraph.lisp — tests for the paragraph widget, mostly ported from ratatui's.

(defpackage #:tui/tests/widgets-paragraph
  (:use #:cl)
  (:shadowing-import-from #:tui/widgets/block #:block)
  (:import-from #:tui/widgets/paragraph #:paragraph #:paragraph-line-count)
  (:import-from #:tui/tests/harness #:deftest #:is #:signals)
  (:import-from #:tui/tests/widgets
                #:str #:blank-buffer #:render-buffer #:render-rows #:row-fgs #:row-bgs
                #:row-modifiers #:repeat)
  (:import-from #:tui/layout #:make-rect)
  (:import-from #:tui/style
                #:make-style #:+reset+ #:+red+ #:+green+ #:+blue+ #:+bold+ #:+italic+)
  (:import-from #:tui/text #:span #:make-line #:make-text)
  (:import-from #:tui/buffer #:buffer-row-strings #:set-style #:cell-wide-p)
  (:import-from #:tui/widgets/widget #:render))

(in-package #:tui/tests/widgets-paragraph)

(defun variants (text &rest options)
  "TEXT as a truncated, a wrapped and a wrapped-and-trimmed paragraph with OPTIONS."
  (list (apply #'paragraph text options)
        (apply #'paragraph text :wrap t :trim nil options)
        (apply #'paragraph text :wrap t options)))

(defun rows (paragraph width height)
  (render-rows paragraph width height))

(defun lines (&rest lines)
  (format nil "~{~A~^~%~}" lines))

(deftest zero-width-character-at-end-of-line
  (dolist (p (variants (str "foo" #x200B)))
    (is (equal '("foo") (rows p 3 1)))
    (is (equal '("foo   ") (rows p 6 1)))
    (is (equal '("foo   " "      ") (rows p 6 2)))
    (is (equal '("foo" "   ") (rows p 3 2)))))

(deftest empty-paragraph
  (dolist (p (variants ""))
    (is (equal '(" ") (rows p 1 1)))
    (is (equal '("          ") (rows p 10 1)))
    (is (equal (repeat 10 "     ") (rows p 5 10)))
    (is (equal '(" " " ") (rows p 1 2)))))

(deftest single-line-paragraph
  (dolist (p (variants "Hello, world!"))
    (is (equal '("Hello, world!  ") (rows p 15 1)))
    (is (equal '("Hello, world!") (rows p 13 1)))
    (is (equal '("Hello, world!  " "               ") (rows p 15 2)))
    (is (equal '("Hello, world!" "             ") (rows p 13 2)))))

(deftest multi-line-paragraph
  (dolist (p (variants (lines "This is a" "multiline" "paragraph.")))
    (is (equal '("This is a " "multiline " "paragraph.") (rows p 10 3)))
    (is (equal '("This is a      " "multiline      " "paragraph.     ") (rows p 15 3)))
    (is (equal '("This is a      " "multiline      " "paragraph.     " "               "
                 "               ")
               (rows p 15 5)))))

(deftest paragraph-with-block
  (destructuring-bind (truncated wrapped trimmed)
      (variants "Hello, worlds!" :block (block :borders :all :title "Title"))
    (dolist (p (list truncated wrapped trimmed))
      (is (equal '("┌Title─────────┐" "│Hello, worlds!│" "└──────────────┘") (rows p 16 3)))
      (is (equal '("┌Title───────────┐" "│Hello, worlds!  │" "└────────────────┘")
                 (rows p 18 3)))
      (is (equal '("┌Title────────────┐" "│Hello, worlds!   │" "│                 │"
                   "└─────────────────┘")
                 (rows p 19 4))))
    (is (equal '("┌Title───────┐" "│Hello, world│" "│            │" "└────────────┘")
               (rows truncated 14 4)))
    (dolist (p (list wrapped trimmed))
      (is (equal '("┌Title──────┐" "│Hello,     │" "│worlds!    │" "└───────────┘")
                 (rows p 13 4))))))

(deftest paragraph-with-bottom-title-and-border
  (is (equal '("Hello, world!  " "Title──────────")
             (rows (paragraph "Hello, world!"
                              :block (block :borders '(:bottom) :title-position :bottom
                                            :title "Title"))
                   15 2))))

(deftest word-wrapping
  (let ((text (str "This is a long line of text that should wrap      and contains a "
                   "superultramegagigalong word.")))
    (destructuring-bind (truncated wrapped trimmed) (variants text)
      (declare (ignore truncated))
      (dolist (p (list wrapped trimmed))
        (is (equal '("This is a long line" "of text that should" "wrap      and      "
                     "contains a         " "superultramegagigal" "ong word.          ")
                   (rows p 19 6))))
      (is (equal '("This is a   " "long line of" "text that   " "should wrap " "    and     "
                   "contains a  " "superultrame" "gagigalong  " "word.       ")
                 (rows wrapped 12 9)))
      (is (equal '("This is a   " "long line of" "text that   " "should wrap " "and contains"
                   "a           " "superultrame" "gagigalong  " "word.       ")
                 (rows trimmed 12 9))))))

(deftest wrapping-whitespace-only-lines
  (let ((text (make-text '("A" "  " "B" "  a" "C"))))
    (is (equal '("A  " "   " "B  " "  a" "C  ") (rows (paragraph text :wrap t :trim nil) 3 5)))
    (is (equal '("A  " "   " "B  " "a  " "C  ") (rows (paragraph text :wrap t) 3 5)))))

(deftest truncation
  (let ((p (paragraph "This is a long line of text that should be truncated.")))
    (is (equal '("This is a long line of") (rows p 22 1)))
    (is (equal '("This is a long line of te") (rows p 25 1)))
    (is (equal '("This is a long line of ") (rows p 23 1)))
    (is (equal '("is is a long line of te")
               (rows (paragraph "This is a long line of text that should be truncated."
                                :scroll-x 2)
                     23 1)))))

(deftest alignment
  (destructuring-bind (truncated wrapped trimmed) (variants "Hello, world!" :alignment :left)
    (dolist (p (list truncated wrapped trimmed))
      (is (equal '("Hello, world!  ") (rows p 15 1))))
    (is (equal '("Hello, wor") (rows truncated 10 1)))
    (is (equal '("Hello,    " "world!    ") (rows wrapped 10 2)))
    (is (equal '("Hello,    " "world!    ") (rows trimmed 10 2))))
  (destructuring-bind (truncated wrapped trimmed) (variants "Hello, world!" :alignment :center)
    (dolist (p (list truncated wrapped trimmed))
      (is (equal '(" Hello, world! ") (rows p 15 1)))
      (is (equal '("  Hello, world! ") (rows p 16 1)))
      (is (equal '("  Hello, world!  ") (rows p 17 1)))
      (is (equal '("Hello, world!") (rows p 13 1))))
    (is (equal '("Hello, wor") (rows truncated 10 1)))
    (is (equal '("  Hello,  " "  world!  ") (rows wrapped 10 2)))
    (is (equal '("  Hello,  " "  world!  ") (rows trimmed 10 2))))
  (destructuring-bind (truncated wrapped trimmed) (variants "Hello, world!" :alignment :right)
    (dolist (p (list truncated wrapped trimmed))
      (is (equal '("  Hello, world!") (rows p 15 1))))
    (is (equal '("Hello, wor") (rows truncated 10 1)))
    (is (equal '("    Hello," "    world!") (rows wrapped 10 2)))
    (is (equal '("    Hello," "    world!") (rows trimmed 10 2)))))

(deftest alignment-comes-from-the-text-and-lines
  (is (equal '("   hi") (rows (paragraph (make-text "hi" :alignment :right)) 5 1)))
  (is (equal '("hi   " "   yo")
             (rows (paragraph (list "hi" (make-line "yo" :alignment :right))) 5 2)))
  (is (equal '("hi   ") (rows (paragraph (make-text "hi" :alignment :right) :alignment :left)
                              5 1))))

(deftest scrolling
  (destructuring-bind (truncated wrapped trimmed)
      (variants (lines "This is a" "cool" "multiline" "paragraph.") :scroll-y 2)
    (dolist (p (list truncated wrapped trimmed))
      (is (equal '("multiline   " "paragraph.  " "            ") (rows p 12 3)))
      (is (equal '("multiline   ") (rows p 12 1))))
    (is (equal '("cool   " "multili" "ne     ") (rows wrapped 7 3))))
  (is (equal '("iline   " "graph.  ")
             (rows (paragraph (lines "This is a" "cool" "multiline" "paragraph.")
                              :scroll-y 2 :scroll-x 4)
                   8 2)))
  ;; Scrolling past the end shows nothing; horizontal scrolling ignores centered lines.
  (is (equal '("    ") (rows (paragraph "abc" :scroll-x 5) 4 1)))
  (is (equal '("    ") (rows (paragraph "abc" :scroll-y 1 :wrap t) 4 1)))
  (is (equal '(" abc ") (rows (paragraph "abc" :scroll-x 2 :alignment :center) 5 1)))
  ;; A wide character cut by the left edge leaves a blank.
  (is (equal (list (str " " #x672C "x"))
             (rows (paragraph (str #x65E5 #x672C "x") :scroll-x 1) 4 1))))

(deftest empty-areas
  (dolist (area (list (make-rect 0 0 0 3) (make-rect 0 0 10 0)))
    (dolist (p (append (variants "Hello, world!") (variants "Hello, world!" :scroll-x 4
                                                                           :scroll-y 2)))
      (let ((buffer (blank-buffer 10 3)))
        (render p area buffer)
        (is (equal (repeat 3 "          ") (buffer-row-strings buffer)))))))

(deftest unicode
  (destructuring-bind (truncated wrapped trimmed)
      (variants (str #x3053 #x3093 #x306B #x3061 #x306F ", " #x4E16 #x754C "! " #x1F603))
    (dolist (p (list truncated wrapped trimmed))
      (is (equal (list (str #x3053 #x3093 #x306B #x3061 #x306F ", " #x4E16 #x754C "! " #x1F603))
                 (rows p 20 1)))
      (is (equal (list (str #x3053 #x3093 #x306B #x3061 #x306F ", " #x4E16 #x754C "! " #x1F603
                            "     "))
                 (rows p 25 1))))
    (is (equal (list (str #x3053 #x3093 #x306B #x3061 #x306F ", " #x4E16 " "))
               (rows truncated 15 1)))
    (dolist (p (list wrapped trimmed))
      (is (equal (list (str #x3053 #x3093 #x306B #x3061 #x306F ",    ")
                       (str #x4E16 #x754C "! " #x1F603 "       "))
                 (rows p 15 2))))))

(deftest wide-characters-and-background
  (let ((buffer (render-buffer (paragraph (str #x3042 #x3044 #x3046)
                                          :style (make-style :bg :green))
                               10 1)))
    (is (cell-wide-p buffer 0 0))
    (is (cell-wide-p buffer 4 0))
    (is (equal (repeat 10 +green+) (row-bgs buffer 0)))))

(deftest out-of-bounds-areas
  (dolist (area (list (make-rect 0 5 15 1) (make-rect 20 0 15 1) (make-rect 20 5 15 1)))
    (let ((buffer (blank-buffer 10 3)))
      (render (paragraph "Beyond the pale") area buffer)
      (is (equal (repeat 3 "          ") (buffer-row-strings buffer)))))
  (let ((buffer (blank-buffer 15 3)))
    (render (paragraph "Hello World") (make-rect 10 0 10 3) buffer)
    (is (equal '("          Hello" "               " "               ")
               (buffer-row-strings buffer))))
  (is (equal '("L") (rows (paragraph "Lorem ipsum") 1 1)))
  (is (equal '() (rows (paragraph "Lorem ipsum") 0 0))))

(deftest line-count
  (is (= 1 (paragraph-line-count (paragraph "Hello World") 20)))
  (is (= 1 (paragraph-line-count (paragraph "Hello World") 10)))
  (is (= 2 (paragraph-line-count (paragraph "Hello World" :wrap t) 10)))
  (is (= 2 (paragraph-line-count (paragraph "Hello World" :wrap t :trim nil) 10)))
  (is (= 0 (paragraph-line-count (paragraph "Hello World") 0)))
  (let ((text (string-right-trim " " (apply #'str (repeat 100 "Hello World ")))))
    (is (= 1 (paragraph-line-count (paragraph text) 11)))
    (is (= 100 (paragraph-line-count (paragraph text :wrap t) 11)))
    (is (= 200 (paragraph-line-count (paragraph text :wrap t :trim nil) 6))))
  (flet ((count-with (borders width &rest options)
           (paragraph-line-count (apply #'paragraph "Hello World"
                                        :block (block :borders borders) options)
                                 width)))
    (is (= 1 (count-with :none 20)))
    (is (= 2 (count-with '(:top) 20)))
    (is (= 2 (count-with '(:bottom) 20)))
    (is (= 3 (count-with :all 10)))
    (is (= 4 (count-with :all 10 :wrap t)))
    (is (= 1 (count-with '(:left :right) 10)))))

;;; Styles

(deftest paragraph-style-patches-what-was-there
  (let ((buffer (blank-buffer 4 1)))
    (set-style buffer (make-rect 0 0 4 1)
               (make-style :fg :red :bg :green :modifiers :italic))
    (render (paragraph "hi" :style (make-style :fg :blue)) (make-rect 0 0 4 1) buffer)
    (is (equal (repeat 4 +blue+) (row-fgs buffer 0)))
    (is (equal (repeat 4 +green+) (row-bgs buffer 0)))
    (is (equal (repeat 4 +italic+) (row-modifiers buffer 0)))))

(deftest text-line-and-span-styles-patch-only-their-cells
  ;; The text's style over the paragraph's.
  (let ((buffer (render-buffer (paragraph (make-text "hi" :style (make-style :fg :blue))
                                          :style (make-style :fg :red :bg :green))
                               4 1)))
    (is (equal (list +blue+ +blue+ +red+ +red+) (row-fgs buffer 0)))
    (is (equal (repeat 4 +green+) (row-bgs buffer 0))))
  ;; The line's over the text's.
  (let ((buffer (render-buffer (paragraph (make-text (make-line "hi" :style (make-style :fg :blue))
                                                     :style (make-style :fg :red :bg :green)))
                               4 1)))
    (is (equal (list +blue+ +blue+ +reset+ +reset+) (row-fgs buffer 0)))
    (is (equal (list +green+ +green+ +reset+ +reset+) (row-bgs buffer 0))))
  ;; The span's over the line's.
  (let ((buffer (render-buffer (paragraph (make-line (span "hi" (make-style :fg :blue))
                                                     :style (make-style :fg :red :bg :green)))
                               4 1)))
    (is (equal (list +blue+ +blue+ +reset+ +reset+) (row-fgs buffer 0)))
    (is (equal (list +green+ +green+ +reset+ +reset+) (row-bgs buffer 0)))))

(deftest paragraph-style-inside-the-block
  (let ((buffer (render-buffer (paragraph "hi" :block (block :borders :all
                                                             :style (make-style :bg :green))
                                               :style (make-style :bg :blue))
                               6 3)))
    (is (equal '("┌────┐" "│hi  │" "└────┘") (buffer-row-strings buffer)))
    (is (equal (repeat 6 +green+) (row-bgs buffer 0)))
    (is (equal (list +green+ +blue+ +blue+ +blue+ +blue+ +green+) (row-bgs buffer 1)))))

(deftest styled-lines-and-spans
  (let* ((line (make-line (list (span "bold" (make-style :modifiers :bold))
                                " and "
                                (span "cyan" (make-style :fg :blue)))))
         (buffer (render-buffer (paragraph (list line "unformatted")) 13 2)))
    (is (equal '("bold and cyan" "unformatted  ") (buffer-row-strings buffer)))
    (is (equal (repeat 4 +bold+ 0 0 0 0 0 0 0 0 0) (row-modifiers buffer 0)))
    (is (equal (repeat 9 +reset+ +blue+ +blue+ +blue+ +blue+) (row-fgs buffer 0))))
  (dolist (p (variants (make-line (list (span "Hello, " (make-style :fg :red))
                                        (span "world!" (make-style :fg :blue))))
                       :style (make-style :bg :green)))
    (let ((buffer (render-buffer p 13 1)))
      (is (equal (repeat 7 +red+ +blue+ +blue+ +blue+ +blue+ +blue+ +blue+)
                 (row-fgs buffer 0)))
      (is (equal (repeat 13 +green+) (row-bgs buffer 0))))))

(deftest bad-arguments-signal-errors
  (signals type-error (paragraph "x" :scroll-x -1))
  (signals type-error (paragraph "x" :alignment :justify))
  (signals type-error (paragraph "x" :block "not a block"))
  (signals error (paragraph 42)))
