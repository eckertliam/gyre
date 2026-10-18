;;;; widgets.lisp — helpers for the widget tests, and tests for the widget protocol, the text
;;;; widgets (strings, spans, lines and texts) and the clear widget.

(defpackage #:tui/tests/widgets
  (:use #:cl)
  (:import-from #:tui/tests/harness #:deftest #:is #:signals)
  (:import-from #:tui/layout #:make-rect #:rect-width #:rect-height)
  (:import-from #:tui/style #:make-style #:+reset+ #:+red+ #:+blue+ #:+bold+ #:+italic+)
  (:import-from #:tui/text #:span #:make-line #:make-text)
  (:import-from #:tui/buffer
                #:make-buffer #:buffer-area #:buffer-row-strings #:set-string #:set-style
                #:set-diff-option #:cell-fg #:cell-bg #:cell-modifiers #:cell-diff-option)
  (:import-from #:tui/render #:frame-buffer #:frame-area)
  (:import-from #:tui/widgets/widget
                #:render #:render-stateful #:render-widget #:render-stateful-widget)
  (:import-from #:tui/widgets/clear #:clear)
  (:export #:str
           #:blank-buffer
           #:filled-buffer
           #:render-buffer
           #:render-rows
           #:row-fgs
           #:row-bgs
           #:row-modifiers
           #:repeat))

(in-package #:tui/tests/widgets)

;;; Helpers

(defun str (&rest parts)
  "Build a string from characters, code points and strings."
  (with-output-to-string (out)
    (dolist (part parts)
      (etypecase part
        (character (write-char part out))
        (integer (write-char (code-char part) out))
        (string (write-string part out))))))

(defun blank-buffer (width height &optional (x 0) (y 0))
  (make-buffer (make-rect x y width height)))

(defun filled-buffer (width height character)
  "A WIDTH × HEIGHT buffer with CHARACTER in every cell."
  (let ((buffer (blank-buffer width height)))
    (dotimes (y height buffer)
      (set-string buffer 0 y (make-string width :initial-element character)))))

(defun render-buffer (widget width height &key (state nil statep) area buffer)
  "Render WIDGET, with STATE if given, into BUFFER (by default a new blank WIDTH × HEIGHT
buffer), in AREA (by default the whole buffer). Return the buffer."
  (let* ((buffer (or buffer (blank-buffer width height)))
         (area (or area (buffer-area buffer))))
    (if statep
        (render-stateful widget area buffer state)
        (render widget area buffer))
    buffer))

(defun render-rows (widget width height &rest options &key state area buffer)
  "Render WIDGET as RENDER-BUFFER does and return the rows of the buffer as strings."
  (declare (ignore state area buffer))
  (buffer-row-strings (apply #'render-buffer widget width height options)))

(defun row-cells (reader buffer y)
  (let ((area (buffer-area buffer)))
    (loop for x from 0 below (rect-width area) collect (funcall reader buffer x y))))

(defun row-fgs (buffer y) (row-cells #'cell-fg buffer y))
(defun row-bgs (buffer y) (row-cells #'cell-bg buffer y))
(defun row-modifiers (buffer y) (row-cells #'cell-modifiers buffer y))

(defun repeat (count item &rest more)
  "A list of COUNT ITEMs followed by MORE."
  (append (make-list count :initial-element item) more))

;;; Text widgets

(deftest strings-render-on-the-first-row
  (is (equal '("hello     " "          ") (render-rows "hello" 10 2)))
  (is (equal '("hel") (render-rows "hello" 3 1)))
  (is (equal '("  hi " "     ")
             (render-rows "hi" 5 2 :area (make-rect 2 0 3 2))))
  (is (equal '("   ") (render-rows "hi" 3 1 :area (make-rect 1 0 0 1))))
  ;; Partly outside the buffer: clipped.
  (is (equal '("    h") (render-rows "hello" 5 1 :area (make-rect 4 0 10 1))))
  (is (equal (list (str #x65E5 " ")) (render-rows (str #x65E5 #x672C) 3 1))))

(deftest spans-render-in-their-style
  (let ((buffer (render-buffer (span "ab" (make-style :fg :red)) 4 1)))
    (is (equal '("ab  ") (buffer-row-strings buffer)))
    (is (equal (list +red+ +red+ +reset+ +reset+) (row-fgs buffer 0)))))

(deftest lines-style-their-row-and-align
  (let ((buffer (render-buffer (make-line (list "a" (span "b" (make-style :fg :blue)))
                                          :style (make-style :bg :red) :alignment :center)
                               6 2)))
    (is (equal '("  ab  " "      ") (buffer-row-strings buffer)))
    (is (equal (repeat 6 +red+) (row-bgs buffer 0)))
    (is (equal (repeat 6 +reset+) (row-bgs buffer 1)))
    (is (equal (list +reset+ +reset+ +reset+ +blue+ +reset+ +reset+) (row-fgs buffer 0))))
  (is (equal '("    ab") (render-rows (make-line "ab" :alignment :right) 6 1)))
  ;; Too wide: keeps the left part, the middle or the right part.
  (is (equal '("Hello") (render-rows (make-line "Hello world") 5 1)))
  (is (equal '("lo wo") (render-rows (make-line "Hello world" :alignment :center) 5 1)))
  (is (equal '("world") (render-rows (make-line "Hello world" :alignment :right) 5 1)))
  ;; A wide character cut at the left edge leaves a blank.
  (is (equal (list (str " " #x672C)) (render-rows (make-line (str #x65E5 #x672C)
                                                             :alignment :right)
                                                  3 1)))
  ;; An empty line draws nothing, not even its style.
  (let ((buffer (render-buffer (make-line "" :style (make-style :bg :red)) 3 1)))
    (is (equal (repeat 3 +reset+) (row-bgs buffer 0)))))

(deftest texts-style-their-area-and-align-each-line
  (let* ((text (make-text (list (make-line "left")
                                (make-line "mid" :alignment :center)
                                "right")
                          :alignment :right :style (make-style :fg :red)))
         (buffer (render-buffer text 7 4)))
    (is (equal '("   left" "  mid  " "  right" "       ") (buffer-row-strings buffer)))
    (is (equal (repeat 7 +red+) (row-fgs buffer 3))))
  (is (equal '("one" "two") (render-rows (make-text (format nil "one~%two~%three")) 3 2))))

(deftest nil-renders-nothing
  (is (equal '("  ") (render-rows nil 2 1))))

(deftest render-widget-defaults-to-the-frame-area
  (let ((frame (tui/render::make-frame))
        (buffer (blank-buffer 4 2)))
    (setf (frame-buffer frame) buffer
          (frame-area frame) (buffer-area buffer))
    (render-widget frame "ab")
    (render-widget frame "cd" (make-rect 2 1 2 1))
    (is (equal '("ab  " "  cd") (buffer-row-strings buffer)))
    (signals error (render-stateful-widget frame "ab" nil))))

;;; Clear

(deftest clear-resets-its-area
  (let ((buffer (filled-buffer 15 7 #\x)))
    (set-style buffer (make-rect 0 0 15 7) (make-style :fg :red :modifiers '(:bold :italic)))
    (set-diff-option buffer (make-rect 0 0 15 7) :skip)
    (render (clear) (make-rect 1 2 3 4) buffer)
    (is (equal '("xxxxxxxxxxxxxxx" "xxxxxxxxxxxxxxx" "x   xxxxxxxxxxx" "x   xxxxxxxxxxx"
                 "x   xxxxxxxxxxx" "x   xxxxxxxxxxx" "xxxxxxxxxxxxxxx")
               (buffer-row-strings buffer)))
    (is (= +reset+ (cell-fg buffer 1 2)))
    (is (= 0 (cell-modifiers buffer 3 5)))
    (is (eq :none (cell-diff-option buffer 2 3)))
    (is (= +red+ (cell-fg buffer 4 2)))
    (is (= (logior +bold+ +italic+) (cell-modifiers buffer 0 0)))
    (is (eq :skip (cell-diff-option buffer 0 0)))))

(deftest clear-clips-to-the-buffer
  (let ((buffer (filled-buffer 15 7 #\x)))
    (render (clear) (make-rect 2 0 100 100) buffer)
    (is (equal (make-list 7 :initial-element "xx             ") (buffer-row-strings buffer))))
  (let ((buffer (filled-buffer 15 7 #\x)))
    (render (clear) (make-rect 100 0 100 100) buffer)
    (is (equal (make-list 7 :initial-element "xxxxxxxxxxxxxxx") (buffer-row-strings buffer)))))

(deftest clear-blanks-both-halves-of-wide-characters
  (let ((buffer (blank-buffer 4 1)))
    (set-string buffer 0 0 (str #x65E5 #x672C))
    (render (clear) (make-rect 1 0 2 1) buffer)
    (is (equal '("    ") (buffer-row-strings buffer)))))
