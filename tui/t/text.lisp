;;;; text.lisp — tests for spans, lines and texts: construction, width, alignment, ranges and
;;;; truncation.

(defpackage #:tui/tests/text
  (:use #:cl)
  (:import-from #:tui/tests/harness #:deftest #:is #:signals)
  (:import-from #:tui/style #:make-style #:patch-style #:style=)
  (:import-from #:tui/text
                #:invalid-alignment #:invalid-text-designator
                #:span #:spanp #:span-content #:span-style
                #:linep #:make-line #:line-spans #:line-style #:line-alignment
                #:textp #:make-text #:text-from-string #:text-lines #:text-style
                #:text-alignment
                #:span-width #:line-width #:text-width #:text-height
                #:alignment-offset #:effective-alignment)
  (:import-from #:tui/text/range #:map-line-range #:copy-line-range)
  (:import-from #:tui/text/truncate #:line-truncation #:line-skip #:truncate-line))

(in-package #:tui/tests/text)

(defun str (&rest parts)
  "Build a string from characters, code points and strings."
  (with-output-to-string (out)
    (dolist (part parts)
      (etypecase part
        (character (write-char part out))
        (integer (write-char (code-char part) out))
        (string (write-string part out))))))

(defun contents (line)
  "The contents of LINE's spans, as a list."
  (map 'list #'span-content (line-spans line)))

(defun text-contents (text)
  "The contents of the spans of each line of TEXT, as a list of lists."
  (map 'list #'contents (text-lines text)))

(defun span-styles (line)
  (map 'list #'span-style (line-spans line)))

(defparameter *red* (make-style :fg :red))
(defparameter *bold* (make-style :modifiers :bold))

;;; Construction

(deftest span-construction
  (let ((s (span "abc")))
    (is (spanp s))
    (is (string= "abc" (span-content s)))
    (is (null (span-style s))))
  (is (eq *red* (span-style (span "x" *red*))))
  (let ((content "shared"))
    (is (eq content (span-content (span content)))))
  (let ((s (span (make-array 3 :element-type 'character :fill-pointer 2
                               :initial-contents "abc"))))
    (is (typep (span-content s) 'simple-string))
    (is (string= "ab" (span-content s))))
  (signals invalid-text-designator (span 42))
  (signals invalid-text-designator (span #\a))
  (signals type-error (span "x" :red)))

(deftest line-designators
  (let ((s (span "a")))
    (is (eq s (svref (line-spans (make-line s)) 0))))
  (is (equal '("abc") (contents (make-line "abc"))))
  (let ((line (make-line (list "ab" (span "cd" *red*) "ef"))))
    (is (linep line))
    (is (equal '("ab" "cd" "ef") (contents line)))
    (is (equal (list nil *red* nil) (span-styles line))))
  (is (equal '("x" "y") (contents (make-line (vector "x" (span "y"))))))
  (is (typep (line-spans (make-line (list "a"))) 'simple-vector))
  (is (equal '() (contents (make-line '()))))
  ;; Newlines in a string are not interpreted.
  (is (equal (list (str "a" #\Newline "b")) (contents (make-line (str "a" #\Newline "b")))))
  (let ((line (make-line "a" :style *red* :alignment :center)))
    (is (eq *red* (line-style line)))
    (is (eq :center (line-alignment line))))
  (is (null (line-alignment (make-line "a"))))
  (signals invalid-text-designator (make-line 42))
  (signals invalid-text-designator (make-line '("a" 42)))
  (signals invalid-text-designator (make-line (make-line "a")))
  (signals invalid-alignment (make-line "a" :alignment :middle))
  (signals type-error (make-line "a" :style 3)))

(deftest text-designators
  (is (equal '(("one") ("two")) (text-contents (make-text (str "one" #\Newline "two")))))
  (let ((line (make-line "l")))
    (is (eq line (svref (text-lines (make-text line)) 0))))
  (is (equal '(("s")) (text-contents (make-text (span "s")))))
  (let ((text (make-text (list "a" (make-line '("b" "c")) (span "d") (make-text "e")))))
    (is (textp text))
    (is (equal '(("a") ("b" "c") ("d") ("e")) (text-contents text))))
  (is (equal '(("a") ("b") ("c"))
             (text-contents (make-text (vector (str "a" #\Newline "b") "c")))))
  (is (= 0 (text-height (make-text '()))))
  (let ((text (make-text "x" :style *red* :alignment :right)))
    (is (eq *red* (text-style text)))
    (is (eq :right (text-alignment text))))
  (signals invalid-text-designator (make-text 42))
  (signals invalid-text-designator (make-text '(("a"))))
  (signals invalid-alignment (make-text "a" :alignment :top))
  (signals type-error (make-text "a" :style :red)))

(deftest a-text-among-lines-keeps-its-look
  (let* ((inner (make-text (list (make-line "a") (make-line "b" :alignment :left :style *bold*))
                           :style *red* :alignment :center))
         (lines (text-lines (make-text (list inner "c")))))
    (is (eq :center (line-alignment (svref lines 0))))
    (is (style= *red* (line-style (svref lines 0))))
    (is (eq :left (line-alignment (svref lines 1))))
    (is (style= (patch-style *red* *bold*) (line-style (svref lines 1))))
    (is (null (line-alignment (svref lines 2))))
    (is (null (line-style (svref lines 2)))))
  ;; A plain text's lines are reused as they are.
  (let ((line (make-line "a")))
    (is (eq line (svref (text-lines (make-text (make-text line))) 0)))))

(deftest text-from-string-splits-lines
  (is (equal '(("a") ("b")) (text-contents (text-from-string (str "a" 13 10 "b")))))
  (is (equal '(("a")) (text-contents (text-from-string (str "a" 10)))))
  (is (equal '(("a") ("")) (text-contents (text-from-string (str "a" 10 10)))))
  (is (equal '(("") ("a")) (text-contents (text-from-string (str 10 "a")))))
  (is (equal '(("")) (text-contents (text-from-string ""))))
  (is (equal '(("")) (text-contents (text-from-string (str 10)))))
  ;; A carriage return not before a newline stays.
  (is (equal (list (list (str "a" 13))) (text-contents (text-from-string (str "a" 13)))))
  (let ((text (text-from-string (str "ab" 10 "cd") :style *red* :alignment :center)))
    (is (eq *red* (text-style text)))
    (is (eq :center (text-alignment text)))
    (is (every (lambda (line) (and (null (line-style line)) (null (line-alignment line))
                                   (every #'null (span-styles line))
                                   (every (lambda (s) (typep (span-content s) 'simple-string))
                                          (line-spans line))))
               (text-lines text))))
  (signals invalid-text-designator (text-from-string 'foo)))

;;; Width

(deftest span-line-and-text-widths
  (is (= 5 (span-width (span "hello"))))
  (is (= 0 (span-width (span ""))))
  (is (= 6 (span-width (span (str #x65E5 #x672C #x8A9E)))))                 ; 日本語
  (is (= 2 (span-width (span (str #x1F600)))))
  (is (= 2 (span-width (span (str #x1F468 #x200D #x1F469 #x200D #x1F467))))) ; family
  (is (= 2 (span-width (span (str #x2764 #xFE0F)))))                       ; VS16
  (is (= 1 (span-width (span (str "e" #x301)))))
  (is (= 9 (line-width (make-line (list "abc" (span (str #x65E5 #x672C)) (str "e" #x301 "x"))))))
  (is (= 0 (line-width (make-line '()))))
  (let ((text (make-text (list "ab" (str #x65E5 #x672C #x8A9E) ""))))
    (is (= 6 (text-width text)))
    (is (= 3 (text-height text))))
  (is (= 0 (text-width (make-text '())))))

;;; Alignment

(deftest alignment-offsets
  (is (= 0 (alignment-offset :left 3 10)))
  (is (= 0 (alignment-offset nil 3 10)))
  (is (= 3 (alignment-offset :center 3 10)))
  (is (= 3 (alignment-offset :center 4 10)))
  (is (= 1 (alignment-offset :center 2 5)))
  (is (= 1 (alignment-offset :center 1 4)))
  (is (= 7 (alignment-offset :right 3 10)))
  (dolist (alignment '(nil :left :center :right))
    (is (= 0 (alignment-offset alignment 10 10)))
    (is (= 0 (alignment-offset alignment 12 10)))))

(deftest line-alignment-overrides-text-alignment
  (is (eq :right (effective-alignment (make-line "a" :alignment :right) :center)))
  (is (eq :center (effective-alignment (make-line "a") :center)))
  (is (null (effective-alignment (make-line "a") nil))))

;;; Ranges

(defun range-parts (line start-span start-char end-span end-char)
  (let ((parts '()))
    (map-line-range (lambda (span start end)
                      (push (subseq (span-content span) start end) parts))
                    line start-span start-char end-span end-char)
    (nreverse parts)))

(deftest map-line-range-visits-each-part
  (let ((line (make-line '("abc" "" "defg" "hi"))))
    (is (equal '("bc" "de") (range-parts line 0 1 2 2)))
    (is (equal '("abc" "defg" "hi") (range-parts line 0 0 3 2)))
    (is (equal '("abc") (range-parts line 0 0 0 3)))
    (is (equal '("defg") (range-parts line 0 3 2 4)))
    (is (equal '() (range-parts line 2 1 2 1))))
  (is (equal '() (range-parts (make-line '()) 0 0 0 0))))

(deftest copy-line-range-keeps-styles
  (let* ((line (make-line (list (span "abc" *red*) (span "def" *bold*))
                          :style *red* :alignment :right))
         (copy (copy-line-range line 0 1 1 2)))
    (is (equal '("bc" "de") (contents copy)))
    (is (equal (list *red* *bold*) (span-styles copy)))
    (is (eq *red* (line-style copy)))
    (is (eq :right (line-alignment copy)))
    (let ((plain (copy-line-range line 0 0 0 1 :style nil :alignment nil)))
      (is (null (line-style plain)))
      (is (null (line-alignment plain))))
    (is (equal '() (contents (copy-line-range line 1 1 1 1)))))
  ;; Spans wholly inside the range are shared.
  (let* ((s (span "abc"))
         (line (make-line (list s "de"))))
    (is (eq s (svref (line-spans (copy-line-range line 0 0 1 1)) 0)))))

;;; Truncation

(deftest truncation-exact-fit
  (let ((line (make-line "hello")))
    (is (equal '(0 5 5) (multiple-value-list (line-truncation line 5))))
    (is (equal '(0 5 5) (multiple-value-list (line-truncation line 9))))
    (is (equal '(0 3 3) (multiple-value-list (line-truncation line 3))))
    (is (equal '("hello") (contents (truncate-line line 5))))))

(deftest truncation-inside-a-span
  (let ((line (make-line (list (span "hello" *red*) (span " world" *bold*))
                         :style *bold* :alignment :center)))
    (is (equal '(1 2 7) (multiple-value-list (line-truncation line 7))))
    (is (equal '(0 5 5) (multiple-value-list (line-truncation line 5))))
    (let ((cut (truncate-line line 7)))
      (is (equal '("hello" " w") (contents cut)))
      (is (equal (list *red* *bold*) (span-styles cut)))
      (is (eq *bold* (line-style cut)))
      (is (eq :center (line-alignment cut))))))

(deftest truncation-before-a-wide-cluster
  (let ((line (make-line (str "ab" #x65E5 "c"))))
    (is (equal '(0 2 2) (multiple-value-list (line-truncation line 3))))
    (is (equal '(0 3 4) (multiple-value-list (line-truncation line 4))))
    (is (equal '("ab") (contents (truncate-line line 3)))))
  (is (equal '(0 0 0) (multiple-value-list (line-truncation (make-line (str #x65E5)) 1))))
  (let ((line (make-line (str "a" #x1F468 #x200D #x1F469 "b"))))
    (is (equal '(0 1 1) (multiple-value-list (line-truncation line 2))))
    (is (equal '(0 4 3) (multiple-value-list (line-truncation line 3))))))

(deftest truncation-edge-cases
  ;; Zero-width clusters right after the last cluster that fits are kept, as in ratatui.
  (is (equal '(0 4 3) (multiple-value-list (line-truncation (make-line (str "foo" #x200B)) 3))))
  (is (equal '(0 0 0) (multiple-value-list (line-truncation (make-line "abc") 0))))
  (is (equal '(0 0 0) (multiple-value-list (line-truncation (make-line '()) 5))))
  (is (equal '() (contents (truncate-line (make-line "abc") 0))))
  (let ((line (make-line '("abc" "defg"))))
    (is (equal '(1 3 4)
               (multiple-value-list (line-truncation line 4 :start-span 0 :start-char 2))))
    (is (equal '(1 4 2)
               (multiple-value-list (line-truncation line 9 :start-span 1 :start-char 2))))))

(deftest skipping-columns
  (let ((line (make-line (list "ab" (str #x65E5 "c")))))
    (is (equal '(0 0 0) (multiple-value-list (line-skip line 0))))
    (is (equal '(0 1 1) (multiple-value-list (line-skip line 1))))
    (is (equal '(1 0 2) (multiple-value-list (line-skip line 2))))
    ;; The wide cluster straddles column 3, so the line continues at column 4.
    (is (equal '(1 1 4) (multiple-value-list (line-skip line 3))))
    (is (equal '(1 1 4) (multiple-value-list (line-skip line 4))))
    (is (equal '(1 2 5) (multiple-value-list (line-skip line 9))))
    (is (equal '(1 1 3) (multiple-value-list (line-skip line 2 :start-span 0 :start-char 1)))))
  (is (equal '(0 0 0) (multiple-value-list (line-skip (make-line '()) 3)))))

;;; Measuring and truncating don't allocate.

(defvar *sink* 0)

(deftest measuring-and-truncating-do-not-allocate
  (let ((lines (list (make-line "plain ascii text, a little longer than the width")
                     (make-line (list (span (str #x65E5 #x672C "x" #x1F600) *red*)
                                      (span (str "e" #x301 #x2764 #xFE0F " ok"))))
                     (make-line (list "a" (str #x1F468 #x200D #x1F469) (str #x1F1FA #x1F1F8)))))
        (text (make-text (list "one" (str #x65E5 #x672C) "three"))))
    (flet ((exercise ()
             (let ((total 0))
               (flet ((count-part (span start end)
                        (declare (ignore span))
                        (incf total (- end start))))
                 (declare (dynamic-extent #'count-part))
                 (dolist (line lines)
                   (incf total (line-width line))
                   (multiple-value-bind (end-span end-char width) (line-truncation line 7)
                     (incf total (+ end-span end-char width))
                     (map-line-range #'count-part line 0 0 end-span end-char))
                   (multiple-value-bind (span char column) (line-skip line 3)
                     (incf total (+ span char column))))
                 (incf total (+ (text-width text) (text-height text)
                                (alignment-offset :center 3 10))))
               (setf *sink* total))))
      (exercise)
      (let ((before (sb-ext:get-bytes-consed)))
        ;; Enough iterations that any allocation would fill a 64 KB allocation region.
        (dotimes (i 20000)
          (exercise))
        (is (= 0 (- (sb-ext:get-bytes-consed) before)))))))
