;;;; wrap.lisp — tests for word wrapping: ratatui's reflow tests, the fixes made to its
;;;; algorithm, styles, edge cases and allocation. reflow.lisp checks random lines against a
;;;; literal port of ratatui's algorithm.

(defpackage #:tui/tests/wrap
  (:use #:cl)
  (:import-from #:tui/tests/harness #:deftest #:is)
  (:import-from #:tui/style #:make-style #:patch-style #:style=)
  (:import-from #:tui/text
                #:span #:span-content #:span-style #:make-line #:line-spans #:line-style
                #:line-alignment #:make-text #:text-lines)
  (:import-from #:tui/text/truncate #:truncate-line)
  (:import-from #:tui/text/wrap #:map-wrapped-lines #:wrap-line #:wrap-text))

(in-package #:tui/tests/wrap)

(defun str (&rest parts)
  "Build a string from characters, code points and strings."
  (with-output-to-string (out)
    (dolist (part parts)
      (etypecase part
        (character (write-char part out))
        (integer (write-char (code-char part) out))
        (string (write-string part out))))))

(defun line-string (line)
  (apply #'concatenate 'string (map 'list #'span-content (line-spans line))))

(defun contents (line)
  (map 'list #'span-content (line-spans line)))

(defun wrapped (string width &key (trim t))
  "The rows of the text STRING wrapped to WIDTH, as strings."
  (mapcar #'line-string (wrap-text (make-text string) width :trim trim)))

(defun row-widths (string width &key (trim t))
  "The widths MAP-WRAPPED-LINES reports for the rows of the text STRING."
  (loop for line across (text-lines (make-text string))
        nconc (let ((widths '()))
                (map-wrapped-lines (lambda (start-span start-char end-span end-char width)
                                     (declare (ignore start-span start-char end-span end-char))
                                     (push width widths))
                                   line width :trim trim)
                (nreverse widths))))

(defun truncated (string width)
  "Each line of the text STRING truncated to WIDTH, as strings."
  (loop for line across (text-lines (make-text string))
        collect (line-string (truncate-line line width))))

;;; Ratatui's tests (ratatui-widgets/src/reflow.rs), with LineTruncator as TRUNCATE-LINE.

(deftest ratatui-one-line
  (loop for i from 1 below 40
        for text = (make-string i :initial-element #\a)
        do (is (equal (list text) (wrapped text 40)))
           (is (equal (list text) (truncated text 40)))))

(deftest ratatui-short-lines
  (let ((text (format nil "abcdefg~%hijklmno~%pabcdefg~%hijklmn~%opabcdefghijk~%lmnopabcd~%~%~%~
                           efghijklmno"))
        (lines '("abcdefg" "hijklmno" "pabcdefg" "hijklmn" "opabcdefghijk" "lmnopabcd" "" ""
                 "efghijklmno")))
    (is (equal lines (wrapped text 20)))
    (is (equal lines (truncated text 20)))))

(deftest ratatui-long-word
  (let ((text "abcdefghijklmnopabcdefghijklmnopabcdefghijklmnopabcdefghijklmno"))
    (is (equal (list (subseq text 0 20) (subseq text 20 40) (subseq text 40 60) (subseq text 60))
               (wrapped text 20)))
    (is (equal (list (subseq text 0 20)) (truncated text 20)))))

(deftest ratatui-long-sentence
  (let ((text "abcd efghij klmnopabcd efgh ijklmnopabcdefg hijkl mnopab c d e f g h i j k l m n o")
        (multi-space (format nil "abcd efghij    klmnopabcd efgh     ijklmnopabcdefg hijkl ~
                                  mnopab c d e f g h i j k l m n o"))
        (rows '("abcd efghij" "klmnopabcd efgh" "ijklmnopabcdefg" "hijkl mnopab c d e f"
                "g h i j k l m n o")))
    (is (equal rows (wrapped text 20)))
    (is (equal rows (wrapped multi-space 20)))
    (is (equal (list (subseq text 0 20)) (truncated text 20)))))

(deftest ratatui-zero-width
  (let ((text "abcd efghij klmnopabcd efgh ijklmnopabcdefg hijkl mnopab "))
    (is (equal '() (wrapped text 0)))
    (is (equal '("") (truncated text 0)))))

(deftest ratatui-width-of-1
  (let ((text "abcd efghij klmnopabcd efgh ijklmnopabcdefg hijkl mnopab "))
    (is (equal (map 'list #'string (remove #\Space text)) (wrapped text 1)))
    (is (equal '("a") (truncated text 1)))))

(deftest ratatui-width-of-1-double-width-characters
  (let ((text (format nil "コンピュータ上で文字を扱う場合、典型的には文字~%aaa~%~
                           aによる通信を行う場合にその両端点では、")))
    (is (equal '("" "a" "a" "a" "a") (wrapped text 1)))
    (is (equal '("" "a" "a") (truncated text 1)))))

(deftest ratatui-mixed-length
  (is (equal '("abcd efghij" "klmnopabcdefghijklmn" "opabcdefghijkl" "mnopab cdefghi j"
               "klmno")
             (wrapped "abcd efghij klmnopabcdefghijklmnopabcdefghijkl mnopab cdefghi j klmno"
                      20))))

(deftest ratatui-double-width-characters
  (let ((text (format nil "コンピュータ上で文字を扱う場合、典型的には文字~
                           による通信を行う場合にその両端点では、")))
    (is (equal '("コンピュータ上で文字") (truncated text 20)))
    (is (equal '("コンピュータ上で文字" "を扱う場合、典型的に"
                 "は文字による通信を行" "う場合にその両端点で" "は、")
               (wrapped text 20)))
    (is (equal '(20 20 20 20 4) (row-widths text 20)))))

(deftest ratatui-leading-whitespace-removal
  (let ((text "AAAAAAAAAAAAAAAAAAAA    AAA"))
    (is (equal '("AAAAAAAAAAAAAAAAAAAA" "AAA") (wrapped text 20)))
    (is (equal '("AAAAAAAAAAAAAAAAAAAA") (truncated text 20)))))

(deftest ratatui-lots-of-spaces
  (let ((text (make-string 69 :initial-element #\Space)))
    (is (equal '("") (wrapped text 20)))
    (is (equal (list (make-string 20 :initial-element #\Space)) (truncated text 20)))))

(deftest ratatui-char-plus-lots-of-spaces
  (let ((text (str "a" (make-string 69 :initial-element #\Space))))
    (is (equal '("a" "") (wrapped text 20)))
    (is (equal (list (subseq text 0 20)) (truncated text 20)))))

(deftest ratatui-double-width-characters-mixed-with-spaces
  (let ((text (format nil "コンピュ ータ上で文字を扱う場合、 典型的には文 ~
                           字による 通信を行 う場合にその両端点では、")))
    (is (equal '("コンピュ" "ータ上で文字を扱う場" "合、 典型的には文"
                 "字による 通信を行" "う場合にその両端点で" "は、")
               (wrapped text 20)))
    (is (equal '(8 20 17 17 20 4) (row-widths text 20)))))

(deftest ratatui-nbsp
  (let ((text (str "AAAAAAAAAAAAAAA AAAA" #xA0 "AAA")))
    (is (equal (list "AAAAAAAAAAAAAAA" (str "AAAA" #xA0 "AAA")) (wrapped text 20)))
    (is (equal '(15 8) (row-widths text 20)))
    (let ((text (substitute #\Space (code-char #xA0) text)))
      (is (equal '("AAAAAAAAAAAAAAA AAAA" "AAA") (wrapped text 20)))
      (is (equal '(20 3) (row-widths text 20))))))

(deftest ratatui-preserve-indentation
  (is (equal '("AAAAAAAAAAAAAAAAAAAA" "   AAA")
             (wrapped "AAAAAAAAAAAAAAAAAAAA    AAA" 20 :trim nil))))

(deftest ratatui-preserve-indentation-with-wrap
  (is (equal '("AAA AAA" "AAAAA AA" "AAAAAA" " B" "  C" "   D")
             (wrapped (format nil "AAA AAA AAAAA AA AAAAAA~% B~%  C~%   D") 10 :trim nil))))

(deftest ratatui-preserve-indentation-lots-of-whitespace
  (is (equal '("          " "    4" "Indent" "          " "      must" "wrap!")
             (wrapped (format nil "               4 Indent~%                 must wrap!") 10
                      :trim nil))))

(deftest ratatui-zero-width-at-end
  (let ((text (str "foo" #x200B)))
    (is (equal '("foo") (wrapped text 3)))
    (is (equal (list text) (truncated text 3)))))

(deftest ratatui-preserves-line-alignment
  (let ((text (make-text (list (make-line "Something that is left aligned." :alignment :left)
                               (make-line "This is right aligned and half short." :alignment :right)
                               (make-line "This should sit in the center." :alignment :center)))))
    (is (equal '(:left :left :right :right :right :center :center)
               (mapcar #'line-alignment (wrap-text text 20))))))

(deftest ratatui-zero-width-white-space
  (is (equal '("foo" "bar") (wrapped (str "foo" #x200B "bar") 3))))

;;; Where ratatui's results are wrong, and what tui/text/wrap does instead.

(deftest wide-clusters-never-overflow-a-row
  ;; Ratatui gives ("日本") at width 3 and ("ab 日") at width 4: rows one column too wide.
  (is (equal '("日" "本") (wrapped "日本" 3)))
  (is (equal '(2 2) (row-widths "日本" 3)))
  (is (equal '("ab" "日") (wrapped "ab 日" 4)))
  (is (equal '("x" "abc" "日") (wrapped "x abc日" 4))))

(deftest leading-whitespace-makes-no-empty-rows
  ;; Ratatui gives ("abcd" "" "efg" "h"): it counts the spaces it is about to trim.
  (is (equal '("abcd" "efg" "h") (wrapped "abcd   efg h" 4))))

(deftest a-row-without-width-lets-no-word-overflow
  ;; WORD JOINER then ZERO WIDTH SPACE make a row of width 0; ratatui then makes a row "abcd".
  (let ((text (str #x2060 #x200B "abcd")))
    (is (equal (list (str #x2060) "abc" "d") (wrapped text 3)))
    (is (equal '(0 3 1) (row-widths text 3)))))

;;; Other behavior

(deftest wrapping-empty-lines
  (is (equal '("") (wrapped "" 5)))
  (let ((calls '()))
    (is (= 1 (map-wrapped-lines (lambda (&rest row) (push row calls)) (make-line '()) 5)))
    (is (equal '((0 0 0 0 0)) calls)))
  (is (equal '("") (mapcar #'line-string (wrap-line (make-line (str 9 13 10 0)) 5))))
  (is (= 0 (map-wrapped-lines #'list (make-line "abc") 0)))
  (is (= 0 (map-wrapped-lines #'list (make-line "abc") -3))))

(deftest lines-that-exactly-fit
  (is (equal '("abcde") (wrapped "abcde" 5)))
  (is (equal '("ab cd") (wrapped "ab cd" 5)))
  (is (equal '("日本") (wrapped "日本" 4)))
  (is (equal '(4) (row-widths "日本" 4))))

(deftest whitespace-at-the-ends-of-a-line
  ;; The last row keeps trailing whitespace; trimming drops leading whitespace.
  (is (equal '("abc   ") (wrapped "abc   " 10)))
  (is (equal '("abc") (wrapped "  abc" 10)))
  (is (equal '("  abc") (wrapped "  abc" 10 :trim nil)))
  (is (equal '("ab" "cd") (wrapped "ab        cd" 4)))
  (is (equal '("ab" "  " " c" "d") (wrapped "ab     cd" 2 :trim nil))))

(deftest control-characters-take-no-columns-and-join-words
  (is (equal (list (str "ab" 9 "cd")) (wrapped (str "ab" 9 "cd") 5)))
  (is (equal (list (str "ab" 9 "c") "d") (wrapped (str "ab" 9 "cd") 3)))
  (is (equal '(3 1) (row-widths (str "ab" 9 "cd") 3))))

(deftest unshowable-wide-clusters-are-left-out
  ;; At width 1 wide clusters are skipped, as in ratatui, and rows never contain one.
  (is (equal '("a" "b") (wrapped "a日b" 1)))
  (is (equal '("") (wrapped "日本" 1)))
  (is (equal '("a") (wrapped (str #x2060 #x65E5 "a") 1)))
  (is (equal '("a") (wrapped (str "a" #x65E5 #x2060) 1)))
  (is (equal '("a" "b") (wrapped (str "a" #x1F600 " " #x1F600 "b") 1)))
  ;; A skipped cluster among whitespace at a break is dropped with it.
  (is (equal (list (str #x2060) "a") (wrapped (str #x2060 " " #x65E5 #x200B " a") 1 :trim nil))))

(deftest styles-survive-wrapping
  (let* ((red (make-style :fg :red))
         (blue (make-style :fg :blue))
         (bold (make-style :modifiers :bold))
         (line (make-line (list (span "hello " red) (span "wor" blue) (span "ld foo"))
                          :style bold :alignment :center))
         (rows (wrap-line line 8)))
    (is (equal '(("hello") ("wor" "ld") ("foo")) (mapcar #'contents rows)))
    (is (equal (list (list red) (list blue nil) (list nil))
               (mapcar (lambda (row) (map 'list #'span-style (line-spans row))) rows)))
    (is (every (lambda (row) (eq bold (line-style row))) rows))
    (is (every (lambda (row) (eq :center (line-alignment row))) rows))))

(deftest wrap-text-folds-in-the-text-style
  (let* ((red (make-style :fg :red))
         (bold (make-style :modifiers :bold))
         (text (make-text (list (make-line "aaa bbb")
                                (make-line "ccc" :alignment :right :style bold))
                          :style red :alignment :center))
         (rows (wrap-text text 3)))
    (is (equal '("aaa" "bbb" "ccc") (mapcar #'line-string rows)))
    (is (equal '(:center :center :right) (mapcar #'line-alignment rows)))
    (is (style= red (line-style (first rows))))
    (is (style= (patch-style red bold) (line-style (third rows))))))

;;; Wrapping doesn't allocate.

(defvar *sink* 0)

(deftest wrapping-does-not-allocate
  (let ((lines (list (make-line "The quick brown fox jumps over the lazy dog, then naps.")
                     (make-line (list (span "コンピュータ上で " (make-style :fg :red))
                                      (span (str "文字を" #x1F600 "扱う " #x2764 #xFE0F " 場合"))
                                      (span (str "  e" #x301 "tc. ")
                                            (make-style :modifiers :bold))))
                     (make-line (list "abcdefghijklmnopqrstuvwxyz" "   " (str "x" 9 "y"))))))
    (flet ((exercise ()
             (let ((total 0))
               (flet ((count-row (start-span start-char end-span end-char width)
                        (incf total (+ start-span start-char end-span end-char width))))
                 (declare (dynamic-extent #'count-row))
                 (dolist (line lines)
                   (dolist (width '(1 7 20))
                     (map-wrapped-lines #'count-row line width)
                     (map-wrapped-lines #'count-row line width :trim nil))))
               (setf *sink* total))))
      (exercise)
      (let ((before (sb-ext:get-bytes-consed)))
        ;; Enough iterations that any allocation would fill a 64 KB allocation region.
        (dotimes (i 2000)
          (exercise))
        (is (= 0 (- (sb-ext:get-bytes-consed) before)))))))
