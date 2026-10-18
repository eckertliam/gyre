;;;; render.lisp — tests for the buffer diff and for drawing frames.

(defpackage #:tui/tests/render
  (:use #:cl)
  (:import-from #:tui/tests/harness #:deftest #:is #:signals)
  (:import-from #:tui/tests/screen
                #:make-screen #:feed-screen #:screen-mismatches #:screen-text
                #:screen-cell-description #:screen-cursor-x #:screen-cursor-y
                #:screen-cursor-visible #:screen-cursor-shape)
  (:import-from #:tui/tests/pty
                #:with-pty #:pty-master #:pty-slave #:set-pty-size #:read-pty #:drain-pty
                #:octets-string #:esc)
  (:import-from #:tui/layout #:make-rect)
  (:import-from #:tui/style #:make-style #:rgb)
  (:import-from #:tui/buffer
                #:make-buffer #:set-string #:set-cell #:set-style #:set-diff-option
                #:cell-continuation-p #:cell-diff-option)
  (:import-from #:tui/output #:make-output #:output-contents)
  (:import-from #:tui/diff #:make-pen #:write-diff #:write-sgr-reset)
  (:import-from #:tui/terminal
                #:make-terminal #:invalidate #:record-features)
  (:import-from #:tui/render #:draw #:frame-buffer #:frame-area #:set-cursor #:hide-cursor))

(in-package #:tui/tests/render)

(defun str (&rest parts)
  (with-output-to-string (out)
    (dolist (part parts)
      (etypecase part
        (integer (write-char (code-char part) out))
        (string (write-string part out))))))

(defun blank (width height)
  (make-buffer (make-rect 0 0 width height)))

(defun diff-octets (previous next)
  (let ((output (make-output))
        (pen (make-pen)))
    (write-diff output pen previous next)
    (write-sgr-reset output pen)
    (output-contents output)))

(defun diff-string (previous next)
  "The diff from PREVIOUS to NEXT as a string, with ESC shown as ~."
  (octets-string (diff-octets previous next)))

(defun buffer-with (width height &rest writes)
  "A buffer with each (x y string &optional style) of WRITES applied."
  (let ((buffer (blank width height)))
    (loop for (x y string style) in writes
          do (set-string buffer x y string :style style))
    buffer))

;;; Minimal output

(deftest one-changed-cell
  (is (string= "~[2;3Hx" (diff-string (blank 5 2) (buffer-with 5 2 '(2 1 "x"))))))

(deftest contiguous-run-needs-one-move
  (is (string= "~[1;2Habc" (diff-string (blank 5 1) (buffer-with 5 1 '(1 0 "abc"))))))

(deftest separate-changes-need-a-move-each
  (is (string= "~[1;1Ha~[1;4Hb"
               (diff-string (blank 5 1) (buffer-with 5 1 '(0 0 "a") '(3 0 "b"))))))

(deftest unchanged-buffers-produce-nothing
  (let ((b (buffer-with 5 1 '(0 0 "hello"))))
    (is (string= "" (diff-string b (buffer-with 5 1 '(0 0 "hello")))))))

(deftest style-only-change
  (is (string= "~[1;1H~[31ma~[0m"
               (diff-string (buffer-with 3 1 '(0 0 "a"))
                            (buffer-with 3 1 (list 0 0 "a" (make-style :fg :red)))))))

(deftest sgr-only-when-the-style-changes
  (is (string= "~[1;1Ha~[1mb~[0m"
               (diff-string (blank 3 1)
                            (buffer-with 3 1 '(0 0 "a") (list 1 0 "b" (make-style :modifiers :bold))))))
  (is (string= "~[1;1H~[31mab~[0m"
               (diff-string (blank 3 1) (buffer-with 3 1 (list 0 0 "ab" (make-style :fg :red)))))))

(deftest color-encodings
  (flet ((sgr (style) (diff-string (blank 1 1) (buffer-with 1 1 (list 0 0 "x" style)))))
    (is (string= "~[1;1H~[38;2;1;2;3mx~[0m" (sgr (make-style :fg (rgb 1 2 3)))))
    (is (string= "~[1;1H~[48;5;200mx~[0m" (sgr (make-style :bg 200))))
    (is (string= "~[1;1H~[91mx~[0m" (sgr (make-style :fg :light-red))))
    (is (string= "~[1;1H~[44mx~[0m" (sgr (make-style :bg :blue))))
    (is (string= "~[1;1H~[1;3;7mx~[0m" (sgr (make-style :modifiers '(:bold :italic :reverse)))))))

(deftest removing-bold-keeps-dim
  (is (string= "~[1;1H~[1;2ma~[22;2mb~[0m"
               (diff-string (blank 2 1)
                            (buffer-with 2 1 (list 0 0 "a" (make-style :modifiers '(:bold :dim)))
                                         (list 1 0 "b" (make-style :modifiers :dim)))))))

(deftest underline-style-and-color-are-separate-sequences
  (is (string= "~[1;1H~[4:3m~[58:2::1:2:3mx~[0m"
               (diff-string (blank 1 1)
                            (buffer-with 1 1 (list 0 0 "x" (make-style :modifiers :curly-underline
                                                                       :underline-color (rgb 1 2 3))))))))

(deftest wide-graphemes-advance-two-columns
  (is (string= (str "~[1;1H" #x65E5 "x")
               (diff-string (blank 4 1) (buffer-with 4 1 (list 0 0 (str #x65E5 "x")))))))

(deftest vs16-graphemes-clear-their-reserved-column-first
  (is (string= (str "~[1;2H ~[1;1H" #x2764 #xFE0F "~[1;3Hx")
               (diff-string (blank 4 1) (buffer-with 4 1 (list 0 0 (str #x2764 #xFE0F "x")))))))

(deftest skip-and-always-update-cells
  (let ((previous (buffer-with 3 1 '(0 0 "abc")))
        (next (buffer-with 3 1 '(0 0 "xbz"))))
    (set-diff-option next (make-rect 0 0 1 1) :skip)
    (set-diff-option next (make-rect 1 0 1 1) :always-update)
    (is (string= "~[1;2Hbz" (diff-string previous next)))))

;;; Round trips through the screen model

(defun round-trip-mismatches (previous next vs16-width)
  "Draw PREVIOUS on a blank screen, then the diff from PREVIOUS to NEXT. Return the
mismatches after each step, and the skip cells of NEXT that changed."
  (let* ((width (tui/cells:buffer-width next))
         (height (tui/cells:buffer-height next))
         (screen (make-screen width height :vs16-width vs16-width)))
    (feed-screen screen (diff-octets (blank width height) previous))
    (let ((first (screen-mismatches screen previous))
          (skipped (loop for y below height
                         nconc (loop for x below width
                                     when (eq :skip (cell-diff-option next x y))
                                       collect (list x y (screen-cell-description screen x y))))))
      (feed-screen screen (diff-octets previous next))
      (values first
              (screen-mismatches screen next
                                 :ignore (lambda (x y) (eq :skip (cell-diff-option next x y))))
              (remove-if (lambda (entry)
                           (destructuring-bind (x y description) entry
                             (equal description (screen-cell-description screen x y))))
                         skipped)))))

(defun round-trip-ok (previous next)
  (loop for vs16-width in '(2 1)
        always (multiple-value-bind (first second skipped)
                   (round-trip-mismatches previous next vs16-width)
                 (and (null first) (null second) (null skipped)))))

(deftest wide-to-narrow-round-trip
  (is (round-trip-ok (buffer-with 4 1 (list 0 0 (str #x65E5 #x672C)))
                     (buffer-with 4 1 '(0 0 "abcd"))))
  (is (round-trip-ok (buffer-with 4 1 (list 0 0 (str #x65E5) (make-style :bg :blue)))
                     (buffer-with 4 1 '(0 0 "a")))))

(deftest narrow-to-wide-round-trip
  (is (round-trip-ok (buffer-with 4 1 '(0 0 "abcd"))
                     (buffer-with 4 1 (list 1 0 (str #x65E5))))))

(deftest wide-shifted-by-one-column
  (is (round-trip-ok (buffer-with 5 1 (list 1 0 (str #x65E5)))
                     (buffer-with 5 1 (list 0 0 (str #x65E5)))))
  (is (round-trip-ok (buffer-with 5 1 (list 0 0 (str #x65E5 #x672C)))
                     (buffer-with 5 1 (list 1 0 (str #x65E5 #x672C))))))

(deftest vs16-round-trips
  (is (round-trip-ok (buffer-with 4 1 '(0 0 "abcd"))
                     (buffer-with 4 1 (list 0 0 (str #x2764 #xFE0F "x")))))
  (is (round-trip-ok (buffer-with 4 1 (list 0 0 (str #x2764 #xFE0F)))
                     (buffer-with 4 1 (list 1 0 (str #x2764 #xFE0F))))))

(deftest style-change-on-a-wide-grapheme
  (is (round-trip-ok (buffer-with 3 1 (list 0 0 (str #x65E5) (make-style :bg :blue)))
                     (buffer-with 3 1 (list 0 0 (str #x65E5) (make-style :fg :red))))))

;;; Random round trips

(defparameter *pieces*
  (vector "a" "b" "x" " " "-" (str #x65E5) (str #x672C) (str #x1F600) (str #x2764 #xFE0F)
          (str "e" #x301) (str #x1F1FA #x1F1F8) (str #x2500) (str #xE9) (str #x1F468 #x200D #x1F469))
  "Graphemes for random buffers: narrow, wide, VS16, combining, flags and ZWJ sequences.")

(defun random-element (vector state)
  (aref vector (random (length vector) state)))

(defun random-color (state)
  (ecase (random 5 state)
    (0 nil)
    (1 :reset)
    (2 (random 16 state))
    (3 (+ 16 (random 240 state)))
    (4 (rgb (random 256 state) (random 256 state) (random 256 state)))))

(defun random-style (state)
  (make-style :fg (random-color state) :bg (random-color state)
              :underline-color (random-color state)
              :modifiers (random 4096 state)
              :remove-modifiers (if (zerop (random 3 state)) (random 4096 state) 0)))

(defun random-string (state)
  (let ((out (make-string-output-stream)))
    (dotimes (i (1+ (random 6 state)) (get-output-stream-string out))
      (write-string (random-element *pieces* state) out))))

(defun random-rect (state width height)
  (let ((x (random width state)) (y (random height state)))
    (make-rect x y (1+ (random (- width x) state)) (1+ (random (- height y) state)))))

(defun random-operations (state width height)
  "A list of functions that each draw something random into a buffer WIDTH × HEIGHT."
  (loop repeat (random 8 state)
        collect (if (zerop (random 4 state))
                    (let ((rect (random-rect state width height))
                          (style (random-style state)))
                      (lambda (buffer) (set-style buffer rect style)))
                    (let ((x (random width state))
                          (y (random height state))
                          (string (random-string state))
                          (style (random-style state)))
                      (lambda (buffer) (set-string buffer x y string :style style))))))

(defun apply-operations (operations buffer)
  (dolist (operation operations buffer)
    (funcall operation buffer)))

(defun random-buffer (state width height)
  (apply-operations (random-operations state width height) (blank width height)))

(defun unsplit-wide-graphemes-at (buffers x y)
  "Blank wide graphemes that straddle the boundary left of column X in row Y."
  (dolist (buffer buffers)
    (when (and (< 0 x (tui/cells:buffer-width buffer)) (cell-continuation-p buffer x y))
      (set-cell buffer (1- x) y #\Space))))

(defun add-random-diff-options (state previous next width height)
  "Mark random cells of NEXT :always-update and then a random rect :skip, making sure no
wide grapheme straddles the edge of the skipped rect."
  (when (zerop (random 3 state))
    (set-diff-option next (random-rect state width height) :always-update))
  (when (zerop (random 3 state))
    (let ((rect (random-rect state width height)))
      (loop for y from (tui/layout:rect-y rect) below (tui/layout:rect-bottom rect)
            do (unsplit-wide-graphemes-at (list previous next) (tui/layout:rect-x rect) y)
               (unsplit-wide-graphemes-at (list previous next) (tui/layout:rect-right rect) y))
      (set-diff-option next rect :skip))))

(deftest random-round-trips
  (let ((state (sb-ext:seed-random-state 2026))
        (failures '()))
    (dotimes (i 500)
      (let* ((width (1+ (random 12 state)))
             (height (1+ (random 4 state)))
             (previous (random-buffer state width height))
             (next (random-buffer state width height)))
        (add-random-diff-options state previous next width height)
        (unless (round-trip-ok previous next)
          (push i failures))))
    (is (null failures))))

;;; Drawing frames on a terminal

(defun frame-output (pty terminal function)
  (draw terminal function)
  (octets-string (read-pty pty :timeout 0.5)))

(deftest first-frame-redraws-everything
  (with-pty (pty 6 2)
    (let ((terminal (make-terminal (pty-slave pty))))
      ;; A terminal outside WITH-TERMINAL starts with a visible cursor; the frame hides it.
      (is (string= "~[0m~[2J~[1;1Hhi~[?25l"
                   (frame-output pty terminal
                                 (lambda (frame) (set-string (frame-buffer frame) 0 0 "hi")))))
      (is (equalp (make-rect 0 0 6 2)
                  (frame-area (draw terminal (lambda (frame) (declare (ignore frame))))))))))

(deftest unchanged-frames-write-nothing
  (with-pty (pty 6 2)
    (let ((terminal (make-terminal (pty-slave pty)))
          (render (lambda (frame) (set-string (frame-buffer frame) 0 0 "hi"))))
      (draw terminal render)
      (drain-pty pty)
      (is (string= "" (frame-output pty terminal render)))
      (is (string= "~[1;2Ho" (frame-output pty terminal
                                            (lambda (frame)
                                              (set-string (frame-buffer frame) 0 0 "ho"))))))))

(deftest frames-set-the-cursor
  (with-pty (pty 6 2)
    (let ((terminal (make-terminal (pty-slave pty))))
      (draw terminal (lambda (frame) (declare (ignore frame))))
      (drain-pty pty)
      (is (string= "~[2;4H~[6 q~[?25h"
                   (frame-output pty terminal (lambda (frame) (set-cursor frame 3 1 :bar)))))
      (is (string= "" (frame-output pty terminal (lambda (frame) (set-cursor frame 3 1 :bar)))))
      (is (string= "~[1;1H" (frame-output pty terminal (lambda (frame) (set-cursor frame 0 0 :bar)))))
      (is (string= "~[?25l" (frame-output pty terminal #'hide-cursor))))))

(deftest synchronized-output-wraps-frames
  (with-pty (pty 6 2)
    (let ((terminal (make-terminal (pty-slave pty))))
      (record-features terminal :synchronized-output t)
      (draw terminal (lambda (frame) (declare (ignore frame))))
      (drain-pty pty)
      (is (string= "~[?2026h~[1;1Hx~[?2026l"
                   (frame-output pty terminal
                                 (lambda (frame) (set-string (frame-buffer frame) 0 0 "x")))))
      ;; A frame without changes sends nothing, not even the synchronization markers.
      (is (string= "" (frame-output pty terminal
                                    (lambda (frame) (set-string (frame-buffer frame) 0 0 "x"))))))))

(deftest invalidate-and-resize-redraw-everything
  (with-pty (pty 6 2)
    (let ((terminal (make-terminal (pty-slave pty)))
          (render (lambda (frame) (set-string (frame-buffer frame) 0 0 "hi"))))
      (draw terminal render)
      (drain-pty pty)
      (invalidate terminal)
      (is (string= "~[0m~[2J~[1;1Hhi" (frame-output pty terminal render)))
      (set-pty-size pty 8 3)
      (let ((area nil))
        (is (string= "~[0m~[2J~[1;1Hhi"
                     (frame-output pty terminal
                                   (lambda (frame)
                                     (setf area (frame-area frame))
                                     (funcall render frame)))))
        (is (equalp (make-rect 0 0 8 3) area))))))

(deftest drawn-frames-round-trip-through-the-screen-model
  (with-pty (pty 10 3)
    (let ((terminal (make-terminal (pty-slave pty)))
          (screen (make-screen 10 3))
          (state (sb-ext:seed-random-state 7)))
      (dotimes (i 50)
        (let* ((operations (random-operations state 10 3))
               (expected (apply-operations operations (blank 10 3))))
          (feed-screen screen
                       (progn
                         (draw terminal
                               (lambda (frame)
                                 (apply-operations operations (frame-buffer frame))
                                 (set-cursor frame 2 1)))
                         (read-pty pty :timeout 0.5)))
          (is (null (screen-mismatches screen expected)))
          (is (screen-cursor-visible screen))
          (is (= 2 (screen-cursor-x screen)))
          (is (= 1 (screen-cursor-y screen))))))))

;;; Zero allocation

(defvar *sink* nil)

(deftest steady-state-drawing-does-not-allocate
  (with-pty (pty 40 12)
    (let* ((null-fd (sb-posix:open "/dev/null" sb-posix:o-wronly))
           (terminal (make-terminal (pty-slave pty) null-fd))
           (style (make-style :fg :red :bg (rgb 10 20 30) :modifiers :bold))
           (lines (vector "the quick brown fox" (str #x65E5 #x672C #x8A9E #x1F600 "e" #x301)
                          (str #x2764 #xFE0F " flags " #x1F1FA #x1F1F8)))
           (counter 0)
           (unchanged (lambda (frame)
                        (let ((buffer (frame-buffer frame)))
                          (dotimes (y 12)
                            (set-string buffer 0 y (aref lines (mod y 3)) :style style))
                          (set-cursor frame 1 1))))
           (changing (lambda (frame)
                       (funcall unchanged frame)
                       ;; About 10% of the screen changes every frame.
                       (set-string (frame-buffer frame) 0 (mod counter 12)
                                   (aref lines (mod counter 3)) :style style))))
      (record-features terminal :synchronized-output t)
      (unwind-protect
           (flet ((consed (function frames)
                    (dotimes (i 10) (draw terminal function) (incf counter))
                    (let ((before (sb-ext:get-bytes-consed)))
                      (dotimes (i frames)
                        (draw terminal function)
                        (setf counter (logand (1+ counter) #xFFFF)))
                      (- (sb-ext:get-bytes-consed) before))))
             ;; 10000 frames: any allocation of 7 or more bytes per frame fills a 64 KB
             ;; allocation region and shows up in GET-BYTES-CONSED.
             (is (= 0 (consed unchanged 10000)))
             (is (= 0 (consed changing 10000))))
        (sb-posix:close null-fd)))))
