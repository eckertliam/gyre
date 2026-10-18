;;;; examples.lisp — smoke tests that run the examples on a pseudo-terminal.

(defpackage #:tui/tests/examples
  (:use #:cl)
  (:import-from #:tui/tests/harness #:deftest #:is #:signals)
  (:import-from #:tui/tests/pty
                #:with-pty #:pty-slave #:read-pty #:write-pty #:search-octets)
  (:import-from #:tui/tests/screen #:make-screen #:feed-screen #:screen-text)
  (:import-from #:tui/posix #:raw-mode-p))

(in-package #:tui/tests/examples)

(defun run-example (main pty)
  "Run the example function MAIN on PTY in a thread and return the thread."
  (sb-thread:make-thread
   (lambda ()
     (funcall main :input-fd (pty-slave pty) :output-fd (pty-slave pty)
                   :resize-handler nil))))

(defstruct (session (:constructor make-session (pty columns rows)))
  pty
  (screen (make-screen 1 1))
  (output (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t))
  columns rows)

(defun screen-shows (session needle &key (timeout 3))
  "Read the example's output into SESSION's screen model until some row of the screen
contains NEEDLE. Return true if it did before TIMEOUT seconds."
  (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop
      (loop for byte across (read-pty (session-pty session) :timeout 0.1)
            do (vector-push-extend byte (session-output session)))
      (let ((screen (make-screen (session-columns session) (session-rows session))))
        (feed-screen screen (session-output session))
        (when (some (lambda (row) (search needle row)) (screen-text screen))
          (return t)))
      (when (> (get-internal-real-time) deadline)
        (return nil)))))

(defun output-contains (session needle &key (timeout 3))
  "Read the example's output until the raw bytes contain NEEDLE (~ for ESC)."
  (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop until (or (search-octets needle (session-output session))
                    (> (get-internal-real-time) deadline))
          do (loop for byte across (read-pty (session-pty session) :timeout 0.1)
                   do (vector-push-extend byte (session-output session))))
    (search-octets needle (session-output session))))

(deftest the-inspector-shows-decoded-events
  (with-pty (pty 100 30)
    (let ((thread (run-example #'tui/examples/inspector:main pty))
          (session (make-session pty 100 30)))
      ;; Answer feature detection like a terminal with the kitty keyboard protocol.
      (is (output-contains session "~[c"))
      (write-pty pty "~[?0u~[?62c")
      (is (screen-shows session "kitty keyboard protocol: yes"))
      (write-pty pty "~[1;5A")
      (is (screen-shows session "key ctrl+:up"))
      (write-pty pty "~")
      (is (screen-shows session "flushed after 25 ms"))
      (write-pty pty (string (code-char 3)))
      (write-pty pty (string (code-char 3)))
      (is (output-contains session "~[?1049l"))
      (sb-thread:join-thread thread :timeout 5)
      (is (not (raw-mode-p (pty-slave pty)))))))

(deftest the-demo-draws-every-widget-and-responds-to-keys
  (with-pty (pty 100 30)
    (let ((thread (run-example #'tui/examples/demo:main pty))
          (session (make-session pty 100 30)))
      (is (output-contains session "~[c"))
      (write-pty pty "~[?62c")
      (is (screen-shows session "> Item 1"))
      (is (screen-shows session "Language"))
      (is (screen-shows session "List │ Table │ Text"))
      (write-pty pty "jj")
      (is (screen-shows session "> Item 3"))
      (write-pty pty (string #\Tab))
      (write-pty pty "j")
      (is (screen-shows session "» Rust"))
      (write-pty pty "p")
      (is (screen-shows session " Popup "))
      (write-pty pty "q")
      (is (output-contains session "~[?1049l"))
      (sb-thread:join-thread thread :timeout 5)
      (is (not (raw-mode-p (pty-slave pty)))))))
