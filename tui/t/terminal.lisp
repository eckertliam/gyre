;;;; terminal.lisp — tests for terminal objects, driving two pseudo-terminals at once.

(defpackage #:tui/tests/terminal
  (:use #:cl)
  (:import-from #:tui/tests/harness #:deftest #:is #:signals)
  (:import-from #:tui/tests/pty
                #:with-pty #:pty-slave #:set-pty-size #:read-pty #:drain-pty #:write-pty
                #:octets-string #:search-octets)
  (:import-from #:tui/posix #:raw-mode-p)
  (:import-from #:tui/buffer #:set-string)
  (:import-from #:tui/terminal
                #:make-terminal #:with-terminal #:terminal-size #:terminal-entered-p
                #:terminal-raw-p #:suspend #:resume #:record-features #:flush-output
                #:wait-for-input #:read-input #:terminal-already-entered #:with-raw-mode)
  (:import-from #:tui/render #:draw #:frame-buffer))

(in-package #:tui/tests/terminal)

(defconstant +pendin+ #+darwin #x20000000 #+linux #o40000
  "PENDIN, a local-mode flag the kernel itself sets when canonical mode comes back.")

(defun termios-flags (fd)
  (let ((termios (sb-posix:tcgetattr fd)))
    (list (sb-posix:termios-iflag termios) (sb-posix:termios-oflag termios)
          (sb-posix:termios-cflag termios)
          (logandc2 (sb-posix:termios-lflag termios) +pendin+))))

(defmacro with-two-ptys ((a b) &body body)
  `(with-pty (,a 80 24)
     (with-pty (,b 100 30)
       ,@body)))

(deftest raw-mode-affects-only-its-own-pty
  (with-two-ptys (a b)
    (let ((ta (make-terminal (pty-slave a)))
          (tb (make-terminal (pty-slave b)))
          (original-a (termios-flags (pty-slave a)))
          (original-b (termios-flags (pty-slave b))))
      (is (not (raw-mode-p (pty-slave a))))
      (with-terminal (ta)
        (is (raw-mode-p (pty-slave a)))
        (is (not (raw-mode-p (pty-slave b))))
        (is (terminal-entered-p ta))
        (is (not (terminal-entered-p tb)))
        (with-terminal (tb)
          (is (raw-mode-p (pty-slave a)))
          (is (raw-mode-p (pty-slave b))))
        (is (raw-mode-p (pty-slave a)))
        (is (not (raw-mode-p (pty-slave b))))
        (is (equal original-b (termios-flags (pty-slave b)))))
      (is (equal original-a (termios-flags (pty-slave a))))
      (is (not (terminal-raw-p ta))))))

(deftest sizes-come-from-each-pty
  (with-two-ptys (a b)
    (let ((ta (make-terminal (pty-slave a)))
          (tb (make-terminal (pty-slave b))))
      (is (equal '(80 24) (multiple-value-list (terminal-size ta))))
      (is (equal '(100 30) (multiple-value-list (terminal-size tb))))
      (set-pty-size a 90 20)
      (is (equal '(90 20) (multiple-value-list (terminal-size ta))))
      (is (equal '(100 30) (multiple-value-list (terminal-size tb)))))))

(deftest output-goes-only-to-its-own-pty
  (with-two-ptys (a b)
    (let ((ta (make-terminal (pty-slave a)))
          (tb (make-terminal (pty-slave b))))
      (with-terminal (ta)
        (with-terminal (tb)
          (drain-pty a)
          (drain-pty b)
          (draw ta (lambda (frame) (set-string (frame-buffer frame) 0 0 "AAAA")))
          (draw tb (lambda (frame) (set-string (frame-buffer frame) 0 0 "BBBB")))
          (draw ta (lambda (frame) (set-string (frame-buffer frame) 0 1 "aaaa")))
          (let ((output-a (read-pty a))
                (output-b (read-pty b)))
            (is (search-octets "AAAA" output-a))
            (is (search-octets "aaaa" output-a))
            (is (not (search-octets "BBBB" output-a)))
            (is (search-octets "BBBB" output-b))
            (is (not (search-octets "AAAA" output-b)))
            (is (not (search-octets "aaaa" output-b)))))))))

(deftest entering-and-leaving-write-the-mode-sequences
  (with-pty (a 80 24)
    (let ((ta (make-terminal (pty-slave a))))
      (with-terminal (ta :mouse t)
        (let ((entered (read-pty a)))
          (dolist (sequence '("~[?1049h" "~[?2004h" "~[?1004h" "~[2J" "~[?25l" "~[?1000h"
                              "~[?1002h" "~[?1003h" "~[?1006h"))
            (is (search-octets sequence entered)))
          (is (not (search-octets "~[>" entered)))))
      (let ((left (read-pty a)))
        (dolist (sequence '("~[?1049l" "~[?2004l" "~[?1004l" "~[?25h" "~[?1006l" "~[0m"))
          (is (search-octets sequence left)))))))

(deftest the-kitty-keyboard-protocol-is-pushed-only-when-supported
  (with-pty (a 80 24)
    (let ((ta (make-terminal (pty-slave a))))
      (record-features ta :kitty-keyboard t)
      (with-terminal (ta)
        (is (search-octets "~[>5u" (read-pty a))))
      (is (search-octets "~[<u" (read-pty a)))
      (with-terminal (ta :kitty-keyboard nil)
        (is (not (search-octets "~[>" (read-pty a)))))
      (is (not (search-octets "~[<u" (read-pty a)))))))

(deftest detection-during-with-terminal-pushes-the-kitty-protocol
  (with-pty (a 80 24)
    (let ((ta (make-terminal (pty-slave a))))
      (with-terminal (ta)
        (drain-pty a)
        (record-features ta :kitty-keyboard t :synchronized-output t)
        (flush-output ta)
        (is (search-octets "~[>5u" (read-pty a))))
      (is (search-octets "~[<u" (read-pty a))))))

(deftest the-terminal-is-restored-on-non-local-exit
  (with-pty (a 80 24)
    (let ((ta (make-terminal (pty-slave a))))
      (catch 'out
        (with-terminal (ta)
          (throw 'out nil)))
      (is (not (raw-mode-p (pty-slave a))))
      (is (not (terminal-entered-p ta))))))

(deftest error-exit-restores-raw-mode
  (with-pty (a 80 24)
    (let ((ta (make-terminal (pty-slave a))))
      (handler-case (with-terminal (ta) (error "boom"))
        (simple-error () nil))
      (is (not (raw-mode-p (pty-slave a))))
      (is (not (terminal-entered-p ta)))
      (is (search-octets "~[?1049l" (read-pty a))))))

(deftest nesting-on-the-same-terminal-is-an-error
  (with-pty (a 80 24)
    (let ((ta (make-terminal (pty-slave a))))
      (with-terminal (ta)
        (signals terminal-already-entered (with-terminal (ta) nil))
        (is (raw-mode-p (pty-slave a)))))))

(deftest suspend-and-resume
  (with-pty (a 80 24)
    (let ((ta (make-terminal (pty-slave a)))
          (render (lambda (frame) (set-string (frame-buffer frame) 0 0 "hi"))))
      (with-terminal (ta)
        (draw ta render)
        (drain-pty a)
        (suspend ta)
        (is (not (raw-mode-p (pty-slave a))))
        (is (search-octets "~[?1049l" (read-pty a)))
        (resume ta)
        (is (raw-mode-p (pty-slave a)))
        (is (search-octets "~[?1049h" (read-pty a)))
        (draw ta render)
        (is (search-octets "~[2J~[1;1Hhi" (read-pty a))))
      (is (not (raw-mode-p (pty-slave a)))))))

(deftest waiting-and-reading-input
  (with-two-ptys (a b)
    (let ((ta (make-terminal (pty-slave a)))
          (tb (make-terminal (pty-slave b)))
          (octets (make-array 16 :element-type '(unsigned-byte 8))))
      (with-raw-mode (ta)
        (with-raw-mode (tb)
          (write-pty a "x")
          (is (wait-for-input ta 1))
          (is (= 1 (read-input ta octets)))
          (is (= (char-code #\x) (aref octets 0)))
          (is (not (wait-for-input tb 0.05)))))
      (is (not (raw-mode-p (pty-slave a)))))))
