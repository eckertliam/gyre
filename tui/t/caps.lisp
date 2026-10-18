;;;; caps.lisp — tests for feature detection, on two pseudo-terminals at once.

(defpackage #:tui/tests/caps
  (:use #:cl)
  (:import-from #:tui/tests/harness #:deftest #:is #:signals)
  (:import-from #:tui/tests/pty
                #:with-pty #:pty-slave #:read-pty #:write-pty #:drain-pty #:octets
                #:search-octets)
  (:import-from #:tui/input
                #:make-decoder #:feed-decoder #:key-event-p #:key-event-key)
  (:import-from #:tui/terminal
                #:make-terminal #:with-terminal #:with-raw-mode #:read-input #:wait-for-input
                #:kitty-keyboard-supported-p #:synchronized-output-supported-p
                #:features-detected-p)
  (:import-from #:tui/caps
                #:start-feature-detection #:handle-feature-reply #:feature-detection-pending-p
                #:finish-feature-detection #:detect-features))

(in-package #:tui/tests/caps)

(defun pump (terminal decoder)
  "Read the input waiting on TERMINAL, decode it with DECODER and pass each event to
HANDLE-FEATURE-REPLY. Return the events it didn't consume."
  (let ((octets (make-array 1024 :element-type '(unsigned-byte 8)))
        (others '()))
    (loop while (wait-for-input terminal 0.2)
          do (dolist (event (feed-decoder decoder octets :end (read-input terminal octets)))
               (unless (handle-feature-reply terminal event)
                 (push event others)))
          until (not (feature-detection-pending-p terminal)))
    (nreverse others)))

(deftest queries-are-sent-to-the-terminal
  (with-pty (a 80 24)
    (let ((ta (make-terminal (pty-slave a))))
      (with-raw-mode (ta)
        (start-feature-detection ta)
        (let ((queries (read-pty a)))
          (is (search-octets "~[?u" queries))
          (is (search-octets "~[?2026$p" queries))
          ;; Primary device attributes come last: their reply ends detection.
          (is (eql (search (octets "~[c") queries :from-end t) (- (length queries) 3))))
        (is (feature-detection-pending-p ta))
        (finish-feature-detection ta)
        (is (not (feature-detection-pending-p ta)))))))

(deftest replies-on-one-pty-affect-only-that-terminal
  (with-pty (a 80 24)
    (with-pty (b 80 24)
      (let ((ta (make-terminal (pty-slave a)))
            (tb (make-terminal (pty-slave b)))
            (da (make-decoder))
            (db (make-decoder)))
        (with-raw-mode (ta)
          (with-raw-mode (tb)
            (start-feature-detection ta)
            (start-feature-detection tb)
            (drain-pty a)
            (drain-pty b)
            ;; A answers everything; B, like default tmux, only answers device attributes.
            (write-pty a "~[?5u~[?2026;2$y~[?62;22c")
            (write-pty b "~[?62;22c")
            (is (null (pump ta da)))
            (is (null (pump tb db)))))
        (is (features-detected-p ta))
        (is (features-detected-p tb))
        (is (kitty-keyboard-supported-p ta))
        (is (synchronized-output-supported-p ta))
        (is (not (kitty-keyboard-supported-p tb)))
        (is (not (synchronized-output-supported-p tb)))))))

(deftest unrecognized-sync-mode-means-unsupported
  (with-pty (a 80 24)
    (let ((ta (make-terminal (pty-slave a)))
          (decoder (make-decoder)))
      (with-raw-mode (ta)
        (start-feature-detection ta)
        (write-pty a "~[?2026;0$y~[?1;2c")
        (pump ta decoder))
      (is (not (synchronized-output-supported-p ta)))
      (is (not (kitty-keyboard-supported-p ta))))))

(deftest other-input-is-left-for-the-application
  (with-pty (a 80 24)
    (let ((ta (make-terminal (pty-slave a)))
          (decoder (make-decoder)))
      (with-raw-mode (ta)
        (start-feature-detection ta)
        (write-pty a "x~[?1uy~[?62c")
        (let ((others (pump ta decoder)))
          (is (equal '(#\x #\y) (mapcar #'key-event-key others)))))
      (is (kitty-keyboard-supported-p ta)))))

(defun answer-after-queries (pty reply)
  "Start a thread that waits for the device attributes query on PTY and then writes REPLY."
  (sb-thread:make-thread
   (lambda ()
     (loop for output = (read-pty pty :timeout 2)
           until (or (zerop (length output)) (search-octets "~[c" output)))
     (write-pty pty reply))))

(deftest blocking-detection
  (with-pty (a 80 24)
    (let* ((ta (make-terminal (pty-slave a)))
           (thread (answer-after-queries a "q~[?5u~[?2026;1$y~[?62c")))
      (let ((others (detect-features ta (make-decoder) :timeout 2)))
        (sb-thread:join-thread thread)
        (is (= 1 (length others)))
        (is (key-event-p (first others)))
        (is (eql #\q (key-event-key (first others)))))
      (is (kitty-keyboard-supported-p ta))
      (is (synchronized-output-supported-p ta))
      ;; Detection put the terminal in raw mode only while it ran.
      (is (not (tui/posix:raw-mode-p (pty-slave a)))))))

(deftest blocking-detection-gives-up-at-the-deadline
  (with-pty (a 80 24)
    (let ((ta (make-terminal (pty-slave a)))
          (start (get-internal-real-time)))
      (is (null (detect-features ta (make-decoder) :timeout 0.2)))
      (is (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 1))
      (is (features-detected-p ta))
      (is (not (kitty-keyboard-supported-p ta)))
      (is (not (feature-detection-pending-p ta))))))

(deftest detection-inside-with-terminal-pushes-the-kitty-protocol
  (with-pty (a 80 24)
    (let* ((ta (make-terminal (pty-slave a))))
      (with-terminal (ta)
        (drain-pty a)
        (let ((thread (answer-after-queries a "~[?0u~[?62c")))
          (detect-features ta (make-decoder) :timeout 2)
          (sb-thread:join-thread thread))
        (is (search-octets "~[>5u" (read-pty a))))
      (is (search-octets "~[<u" (read-pty a))))))
