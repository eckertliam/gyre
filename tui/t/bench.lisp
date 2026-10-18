;;;; bench.lisp — drawing benchmarks, reported rather than asserted. Run with `make bench`.

(defpackage #:tui/tests/bench
  (:use #:cl)
  (:import-from #:tui/tests/pty #:with-pty #:pty-slave #:read-pty)
  (:import-from #:tui/style #:make-style #:rgb)
  (:import-from #:tui/buffer #:set-string)
  (:import-from #:tui/terminal #:make-terminal #:invalidate)
  (:import-from #:tui/render #:draw #:frame-buffer)
  (:export #:main))

(in-package #:tui/tests/bench)

(defconstant +columns+ 250)
(defconstant +rows+ 80)

(defun make-lines (state)
  "Rows of random styled text: a mix of ASCII words, some CJK and some emoji."
  (let ((words #("lorem" "ipsum" "dolor" "sit" "amet" "(defun" "draw" "frame)"
                 #.(coerce (list (code-char #x65E5) (code-char #x672C)) 'string)
                 #.(string (code-char #x1F600)))))
    (coerce (loop repeat +rows+
                  collect (with-output-to-string (out)
                            (loop while (< (file-position out) (* 2 +columns+))
                                  do (write-string (aref words (random (length words) state)) out)
                                     (write-char #\Space out))))
            'vector)))

(defun make-styles (state)
  (coerce (loop repeat 16
                collect (make-style :fg (rgb (random 256 state) (random 256 state) (random 256 state))
                                    :bg (random 16 state)
                                    :modifiers (if (zerop (random 4 state)) :bold 0)))
          'vector))

(defun render-screen (frame lines styles shift)
  "Fill the screen with LINES. The first SHIFT rows start one word later and use another
style, so drawing with SHIFT 8 after SHIFT 0 changes 10% of the rows."
  (let ((buffer (frame-buffer frame)))
    (dotimes (y +rows+)
      (let ((shifted (< y shift)))
        (set-string buffer 0 y (aref lines y)
                    :start (if shifted 6 0)
                    :style (aref styles (mod (if shifted (1+ y) y) (length styles))))))))

(defun full-redraw (terminal lines styles i)
  (invalidate terminal)
  (draw terminal (lambda (frame) (render-screen frame lines styles (mod i 2)))))

(defun partial-change (terminal lines styles i)
  ;; 8 of the 80 rows change from one frame to the next.
  (draw terminal (lambda (frame) (render-screen frame lines styles (* 8 (mod i 2))))))

(defun no-change (terminal lines styles i)
  (declare (ignore i))
  (draw terminal (lambda (frame) (render-screen frame lines styles 0))))

(defun microseconds-per-frame (function lines styles frames)
  "Draw FRAMES frames on a terminal whose output goes to /dev/null."
  (with-pty (pty +columns+ +rows+)
    (let* ((null-fd (sb-posix:open "/dev/null" sb-posix:o-wronly))
           (terminal (make-terminal (pty-slave pty) null-fd)))
      (unwind-protect
           (progn
             (dotimes (i 20) (funcall function terminal lines styles i))
             (let ((start (get-internal-real-time)))
               (dotimes (i frames) (funcall function terminal lines styles i))
               (/ (* 1000000.0 (- (get-internal-real-time) start))
                  internal-time-units-per-second frames)))
        (sb-posix:close null-fd)))))

(defun bytes-per-frame (function lines styles frames)
  "Draw FRAMES frames on a pty and return the mean number of bytes written per frame. A
thread drains the pty so that writing a large frame can't block."
  (with-pty (pty +columns+ +rows+)
    (let* ((terminal (make-terminal (pty-slave pty)))
           (total 0)
           (stop nil)
           (reader (sb-thread:make-thread
                    (lambda ()
                      (loop for octets = (read-pty pty :timeout 0.05)
                            do (incf total (length octets))
                            until (and stop (zerop (length octets))))))))
      (flet ((settled-total ()
               (sleep 0.3)
               total))
        (dotimes (i 2) (funcall function terminal lines styles i))
        (let ((before (settled-total)))
          (dotimes (i frames) (funcall function terminal lines styles i))
          (prog1 (round (- (settled-total) before) frames)
            (setf stop t)
            (sb-thread:join-thread reader)))))))

(defun main ()
  "Print the time and output size of drawing a 250×80 frame that changes completely, one
that changes 10%, and one that doesn't change."
  (let* ((state (sb-ext:seed-random-state 99))
         (lines (make-lines state))
         (styles (make-styles state)))
    (format t "~&Benchmark: ~D×~D buffer (ASCII, CJK, emoji, 24-bit colors), ~
               output written to /dev/null~%" +columns+ +rows+)
    (let ((frame-buffer (tui/buffer:make-buffer (tui/layout:make-rect 0 0 +columns+ +rows+)))
          (start (get-internal-real-time)))
      ;; What the application's render function costs on its own, without diffing or output.
      (dotimes (i 2000)
        (tui/buffer:reset-buffer frame-buffer)
        (dotimes (y +rows+)
          (set-string frame-buffer 0 y (aref lines y) :style (aref styles (mod y 16)))))
      (format t "  ~12A ~8,1F µs/frame  (reset-buffer + SET-STRING for every row; ~
                 included in the times below)~%"
              "render only"
              (/ (* 1000000.0 (- (get-internal-real-time) start)) internal-time-units-per-second
                 2000)))
    (loop for (name function frames) in '(("full redraw" full-redraw 300)
                                          ("10% change" partial-change 2000)
                                          ("unchanged" no-change 2000))
          do (format t "  ~12A ~8,1F µs/frame  ~7D bytes/frame~%"
                     name
                     (microseconds-per-frame function lines styles frames)
                     (bytes-per-frame function lines styles 4))))
  (finish-output))
