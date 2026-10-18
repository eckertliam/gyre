;;;; buffer.lisp — tests for the cell buffer.

(defpackage #:tui/tests/buffer
  (:use #:cl)
  (:import-from #:tui/tests/harness #:deftest #:is #:signals)
  (:import-from #:tui/layout #:make-rect)
  (:import-from #:tui/style #:make-style #:+reset+ #:+bold+ #:+italic+ #:+red+ #:+blue+)
  (:import-from #:tui/buffer
                #:make-buffer #:buffer-area #:resize-buffer #:reset-buffer
                #:position-outside-buffer #:invalid-grapheme #:cell-grapheme #:cell-fg
                #:cell-bg #:cell-modifiers #:cell-wide-p #:cell-continuation-p
                #:cell-diff-option #:set-cell #:set-cell-style #:set-diff-option #:set-string
                #:set-style #:fill-cells #:buffer-row-strings))

(in-package #:tui/tests/buffer)

(defun str (&rest parts)
  (with-output-to-string (out)
    (dolist (part parts)
      (etypecase part
        (integer (write-char (code-char part) out))
        (string (write-string part out))))))

(defun buffer (width height &optional (x 0) (y 0))
  (make-buffer (make-rect x y width height)))

(deftest new-buffers-are-empty
  (let ((b (buffer 3 2)))
    (is (equal '("   " "   ") (buffer-row-strings b)))
    (is (= +reset+ (cell-fg b 2 1)))
    (is (= 0 (cell-modifiers b 0 0)))
    (is (eq :none (cell-diff-option b 0 0)))
    (signals position-outside-buffer (cell-grapheme b 3 0))
    (signals position-outside-buffer (cell-grapheme b 0 -1))))

(deftest set-string-writes-and-clips
  (let ((b (buffer 5 2)))
    (is (= 3 (set-string b 0 0 "abc")))
    (is (= 2 (set-string b 3 0 "xyz")))
    (is (= 2 (set-string b 1 1 "hello" :max-width 2)))
    (is (= 0 (set-string b 0 5 "nope")))
    (is (equal '("abcxy" " he  ") (buffer-row-strings b)))))

(deftest set-string-clips-on-the-left
  (let ((b (buffer 4 1 10 5)))
    (is (= 5 (set-string b 8 5 "abcde")))
    (is (equal '("cde ") (buffer-row-strings b)))))

(deftest set-string-skips-control-characters
  (let ((b (buffer 5 1)))
    (is (= 3 (set-string b 0 0 (str "a" 9 "b" 27 "c"))))
    (is (equal '("abc  ") (buffer-row-strings b)))))

(deftest wide-graphemes-take-two-cells
  (let ((b (buffer 5 1)))
    (is (= 4 (set-string b 0 0 (str #x65E5 #x672C #x8A9E))))   ; the third doesn't fit
    (is (cell-wide-p b 0 0))
    (is (cell-continuation-p b 1 0))
    (is (equal "" (cell-grapheme b 1 0)))
    (is (equal (list (str #x65E5 #x672C " ")) (buffer-row-strings b)))))

(deftest writing-over-half-a-wide-grapheme-clears-the-other-half
  (let ((b (buffer 4 1)))
    (set-string b 0 0 (str #x65E5))
    (set-cell b 1 0 #\x)
    (is (equal '(" x  ") (buffer-row-strings b)))
    (is (not (cell-wide-p b 0 0))))
  (let ((b (buffer 4 1)))
    (set-string b 0 0 (str #x65E5))
    (set-cell b 0 0 #\a)
    (is (equal '("a   ") (buffer-row-strings b)))
    (is (not (cell-continuation-p b 1 0))))
  (let ((b (buffer 4 1)))
    (set-string b 1 0 (str #x65E5))
    (set-string b 0 0 (str #x672C))
    (is (equal (list (str #x672C "  ")) (buffer-row-strings b)))
    (is (not (cell-continuation-p b 2 0)))))

(deftest wide-graphemes-carry-their-style-to-the-continuation
  (let ((b (buffer 3 1)))
    (set-string b 0 0 (str #x65E5) :style (make-style :bg :red))
    (is (= +red+ (cell-bg b 1 0)))))

(deftest multi-code-point-graphemes
  (let ((b (buffer 4 1))
        (heart (str #x2764 #xFE0F))
        (accented (str "e" #x301)))
    (is (= 3 (set-string b 0 0 (str heart accented))))
    (is (equal heart (cell-grapheme b 0 0)))
    (is (cell-wide-p b 0 0))
    (is (equal accented (cell-grapheme b 2 0)))
    (is (not (cell-wide-p b 2 0)))))

(deftest styles-patch-cells
  (let ((b (buffer 3 1)))
    (set-style b (make-rect 0 0 3 1) (make-style :bg :blue :modifiers :bold))
    (set-string b 0 0 "ab" :style (make-style :fg :red :modifiers :italic))
    (is (= +red+ (cell-fg b 0 0)))
    (is (= +blue+ (cell-bg b 0 0)))
    (is (= (logior +bold+ +italic+) (cell-modifiers b 1 0)))
    (is (= +reset+ (cell-fg b 2 0)))
    (set-cell-style b 2 0 (make-style :remove-modifiers :bold))
    (is (= 0 (cell-modifiers b 2 0)))
    (set-style b (make-rect -5 -5 100 100) (make-style :fg :red))
    (is (= +red+ (cell-fg b 2 0)))))

(deftest set-cell-validates-graphemes
  (let ((b (buffer 3 1)))
    (signals invalid-grapheme (set-cell b 0 0 "ab"))
    (signals invalid-grapheme (set-cell b 0 0 ""))
    (signals invalid-grapheme (set-cell b 0 0 (code-char 7)))
    (set-cell b 9 9 #\x)                                ; ignored
    (set-cell b 2 0 (str #x65E5))                       ; doesn't fit, ignored
    (is (equal '("   ") (buffer-row-strings b)))))

(deftest fill-and-reset
  (let ((b (buffer 5 2)))
    (set-string b 0 0 "hello" :style (make-style :fg :red))
    (fill-cells b (make-rect 1 0 3 2) #\- (make-style :bg :blue))
    (is (equal '("h---o" " --- ") (buffer-row-strings b)))
    (is (= +reset+ (cell-fg b 1 0)))
    (is (= +blue+ (cell-bg b 1 1)))
    (fill-cells b (make-rect 0 1 5 1) (str #x65E5))
    (is (equal (list "h---o" (str #x65E5 #x65E5 " ")) (buffer-row-strings b)))
    (reset-buffer b)
    (is (equal '("     " "     ") (buffer-row-strings b)))
    (is (= +reset+ (cell-bg b 1 1)))))

(deftest diff-options
  (let ((b (buffer 3 1)))
    (set-diff-option b (make-rect 1 0 1 1) :skip)
    (set-diff-option b (make-rect 2 0 1 1) :always-update)
    (is (eq :none (cell-diff-option b 0 0)))
    (is (eq :skip (cell-diff-option b 1 0)))
    (is (eq :always-update (cell-diff-option b 2 0)))
    (set-string b 0 0 "abc")
    (is (eq :skip (cell-diff-option b 1 0)))))

(deftest resizing
  (let ((b (buffer 2 2)))
    (set-string b 0 0 "ab")
    (resize-buffer b (make-rect 1 1 3 1))
    (is (equalp (make-rect 1 1 3 1) (buffer-area b)))
    (is (equal '("   ") (buffer-row-strings b)))))

(defvar *sink* nil)

(deftest writing-strings-does-not-allocate
  (let ((b (buffer 40 3))
        (area (make-rect 0 0 10 3))
        (style (make-style :fg :red :modifiers :bold))
        (lines (list "plain ascii text"
                     (str #x65E5 #x672C "x" #x1F600 "e" #x301 #x2764 #xFE0F)
                     (str #x1F468 #x200D #x1F469 #x1F1FA #x1F1F8))))
    (flet ((frame ()
             (reset-buffer b)
             (loop for line in lines for y from 0
                   do (setf *sink* (set-string b 0 y line :style style)))
             (set-style b area style)))
      (frame)
      (let ((before (sb-ext:get-bytes-consed)))
        (dotimes (i 20000) (frame))
        (is (= 0 (- (sb-ext:get-bytes-consed) before)))))))
