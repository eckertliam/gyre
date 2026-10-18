;;;; buffer-text.lisp — tests for writing spans, lines and texts into buffers.

(defpackage #:tui/tests/buffer-text
  (:use #:cl)
  (:import-from #:tui/tests/harness #:deftest #:is #:signals)
  (:import-from #:tui/layout #:make-rect)
  (:import-from #:tui/style #:make-style #:+red+ #:+blue+ #:+bold+ #:+italic+ #:+reset+)
  (:import-from #:tui/text #:span #:make-line #:make-text #:text-from-string)
  (:import-from #:tui/buffer
                #:make-buffer #:buffer-row-strings #:cell-fg #:cell-bg #:cell-modifiers)
  (:import-from #:tui/buffer/text #:set-span #:set-line #:set-text))

(in-package #:tui/tests/buffer-text)

(defun buffer (width height)
  (make-buffer (make-rect 0 0 width height)))

(defun str (&rest parts)
  (with-output-to-string (out)
    (dolist (part parts)
      (etypecase part
        (integer (write-char (code-char part) out))
        (string (write-string part out))))))

(deftest set-span-uses-the-span-style
  (let ((b (buffer 5 1)))
    (is (= 2 (set-span b 1 0 (span "hi" (make-style :fg :red)))))
    (is (equal '(" hi  ") (buffer-row-strings b)))
    (is (= +red+ (cell-fg b 1 0)))
    (is (= +reset+ (cell-fg b 0 0)))))

(deftest set-line-combines-line-and-span-styles
  (let ((b (buffer 8 1))
        (line (make-line (list (span "ab" (make-style :modifiers :bold))
                               (span "cd" (make-style :fg :blue :remove-modifiers :italic)))
                         :style (make-style :fg :red :modifiers :italic))))
    (is (= 4 (set-line b 0 0 line)))
    (is (equal '("abcd    ") (buffer-row-strings b)))
    (is (= +red+ (cell-fg b 0 0)))
    (is (= (logior +bold+ +italic+) (cell-modifiers b 0 0)))
    (is (= +blue+ (cell-fg b 2 0)))
    (is (= 0 (cell-modifiers b 2 0)))))

(deftest set-line-stops-at-max-width
  (let ((b (buffer 8 1)))
    (is (= 3 (set-line b 0 0 (make-line '("ab" "cdef")) :max-width 3)))
    (is (equal '("abc     ") (buffer-row-strings b)))))

(deftest set-text-aligns-lines
  (let ((b (buffer 6 3)))
    (set-text b (make-rect 0 0 6 3)
              (make-text (list (make-line "ab" :alignment :right)
                               (make-line "cd")
                               "ef")
                         :alignment :center))
    (is (equal '("    ab" "  cd  " "  ef  ") (buffer-row-strings b)))))

(deftest set-text-clips-wide-lines-by-alignment
  (flet ((row (alignment)
           (let ((b (buffer 4 1)))
             (set-text b (make-rect 0 0 4 1) (make-text "abcdefgh" :alignment alignment))
             (first (buffer-row-strings b)))))
    (is (string= "abcd" (row :left)))
    (is (string= "cdef" (row :center)))
    (is (string= "efgh" (row :right))))
  ;; A wide cluster cut by the left edge leaves a blank column.
  (let ((b (buffer 3 1)))
    (set-text b (make-rect 0 0 3 1) (make-text (str "a" #x65E5 "bc") :alignment :right))
    (is (equal '(" bc") (buffer-row-strings b)))))

(deftest set-text-styles-the-area-and-clips-rows
  (let ((b (buffer 3 2)))
    (set-text b (make-rect 0 0 3 2) (text-from-string (format nil "one~%two~%six")
                                                      :style (make-style :bg :blue)))
    (is (equal '("one" "two") (buffer-row-strings b)))
    (is (= +blue+ (cell-bg b 2 1)))))
