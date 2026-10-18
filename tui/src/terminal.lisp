;;;; terminal.lisp — a terminal driven through file descriptors it was given.
;;;;
;;;; A terminal object holds everything about one terminal: its descriptors, saved termios,
;;;; the modes it is in, detected features and its output buffer. A process can drive any
;;;; number of them at once. Nothing here installs signal handlers, starts threads, opens
;;;; /dev/tty or touches the standard streams.

(defpackage #:tui/terminal
  (:use #:cl)
  (:import-from #:tui/output
                #:output #:make-output #:output-octets #:output-fill #:reset-output
                #:output-ascii)
  (:import-from #:tui/posix
                #:enter-raw-mode #:restore-termios #:window-size #:wait-readable
                #:read-octets #:write-octets)
  (:export #:terminal
           #:make-terminal
           #:terminal-input-fd
           #:terminal-output-fd
           #:terminal-output
           #:terminal-entered-p
           #:terminal-raw-p
           #:terminal-size
           #:terminal-size-unknown
           #:flush-output
           #:with-terminal
           #:call-with-terminal
           #:suspend
           #:resume
           #:invalidate
           #:terminal-invalid-p
           #:terminal-cursor-visible-p
           #:terminal-cursor-shape
           #:terminal-renderer
           #:terminal-detection
           #:kitty-keyboard-supported-p
           #:synchronized-output-supported-p
           #:features-detected-p
           #:record-features
           #:terminal-already-entered
           #:with-raw-mode
           #:wait-for-input
           #:read-input
           #:+kitty-keyboard-flags+))

(in-package #:tui/terminal)

(defstruct (terminal (:constructor %make-terminal (input-fd output-fd))
                     (:copier nil))
  "One terminal, read from INPUT-FD and drawn on OUTPUT-FD (possibly the same descriptor)."
  (input-fd 0 :type fixnum :read-only t)
  (output-fd 1 :type fixnum :read-only t)
  (output (make-output) :type output :read-only t)
  ;; Settings to restore, while in raw mode.
  (saved-termios nil)
  (state :idle :type (member :idle :entered :suspended))
  ;; Options of the current WITH-TERMINAL.
  (mouse nil :type boolean)
  (kitty-keyboard-wanted nil :type boolean)
  (kitty-keyboard-pushed nil :type boolean)
  ;; Feature detection results (see tui/caps).
  (features-detected nil :type boolean)
  (kitty-keyboard-supported nil :type boolean)
  (synchronized-output-supported nil :type boolean)
  ;; The cursor as last drawn.
  (cursor-visible t :type boolean)
  (cursor-shape :default :type keyword)
  ;; True when the next frame must redraw the whole screen.
  (invalid t :type boolean)
  ;; Owned by tui/render and tui/caps respectively.
  (renderer nil)
  (detection nil))

(defun make-terminal (input-fd &optional (output-fd input-fd))
  "Return a terminal that reads input from the descriptor INPUT-FD and draws on
OUTPUT-FD, which defaults to INPUT-FD. Nothing is done to the terminal until
WITH-TERMINAL."
  (%make-terminal input-fd output-fd))

(defconstant +kitty-keyboard-flags+ 5
  "The kitty keyboard protocol flags pushed when the terminal supports the protocol:
disambiguate escape codes (1) and report alternate keys (4), as Helix does.")

;;; Output

(defun flush-output (terminal)
  "Write everything in TERMINAL's output buffer to its output descriptor, normally with a
single write(2), and empty the buffer."
  (let ((output (terminal-output terminal)))
    (when (plusp (output-fill output))
      (write-octets (terminal-output-fd terminal) (output-octets output)
                    :end (output-fill output))
      (reset-output output))
    terminal))

(defun emit (terminal &rest sequences)
  (dolist (sequence sequences)
    (output-ascii (terminal-output terminal) sequence)))

(defun esc (string)
  "STRING with each ~ replaced by ESC."
  (substitute (code-char 27) #\~ string))

;;; Size

(define-condition terminal-size-unknown (error)
  ((terminal :initarg :terminal :reader terminal-size-unknown-terminal))
  (:report "Neither descriptor of the terminal reports a window size.")
  (:documentation "Signaled when a terminal's size can't be read with TIOCGWINSZ."))

(defun terminal-size (terminal)
  "Return the size of TERMINAL as two values, columns and rows, from TIOCGWINSZ on its
output descriptor, or on its input descriptor if that fails. Signal
TERMINAL-SIZE-UNKNOWN when neither works."
  (multiple-value-bind (columns rows) (window-size (terminal-output-fd terminal))
    (if columns
        (values columns rows)
        (multiple-value-bind (columns rows) (window-size (terminal-input-fd terminal))
          (if columns
              (values columns rows)
              (error 'terminal-size-unknown :terminal terminal))))))

;;; Raw mode

(defun terminal-raw-p (terminal)
  "True while TERMINAL is in raw mode because of this library."
  (not (null (terminal-saved-termios terminal))))

(defun enter-raw (terminal)
  (unless (terminal-saved-termios terminal)
    (setf (terminal-saved-termios terminal) (enter-raw-mode (terminal-input-fd terminal)))))

(defun leave-raw (terminal)
  (let ((termios (terminal-saved-termios terminal)))
    (when termios
      (setf (terminal-saved-termios terminal) nil)
      (restore-termios (terminal-input-fd terminal) termios))))

(defmacro with-raw-mode ((terminal) &body body)
  "Evaluate BODY with TERMINAL in raw mode, restoring its settings afterwards if this
form entered raw mode."
  `(call-with-raw-mode ,terminal (lambda () ,@body)))

(defun call-with-raw-mode (terminal function)
  (if (terminal-raw-p terminal)
      (funcall function)
      (progn
        (enter-raw terminal)
        (unwind-protect (funcall function)
          (leave-raw terminal)))))

;;; Entering and leaving

(defun push-kitty-keyboard (terminal)
  (unless (terminal-kitty-keyboard-pushed terminal)
    (setf (terminal-kitty-keyboard-pushed terminal) t)
    (emit terminal (esc (format nil "~~[>~Du" +kitty-keyboard-flags+)))))

(defun write-enter-sequences (terminal)
  ;; Alternate screen, bracketed paste, focus events, a cleared screen and a hidden cursor.
  (emit terminal (esc "~[?1049h~[?2004h~[?1004h~[2J~[?25l"))
  (when (terminal-mouse terminal)
    ;; Button presses, drags and all motion, reported in SGR (1006) encoding.
    (emit terminal (esc "~[?1000h~[?1002h~[?1003h~[?1006h")))
  (when (and (terminal-kitty-keyboard-wanted terminal)
             (terminal-kitty-keyboard-supported terminal))
    (push-kitty-keyboard terminal))
  (setf (terminal-cursor-visible terminal) nil
        (terminal-cursor-shape terminal) :default
        (terminal-invalid terminal) t))

(defun write-leave-sequences (terminal)
  (when (terminal-kitty-keyboard-pushed terminal)
    (setf (terminal-kitty-keyboard-pushed terminal) nil)
    (emit terminal (esc "~[<u")))
  (when (terminal-mouse terminal)
    (emit terminal (esc "~[?1006l~[?1003l~[?1002l~[?1000l")))
  (emit terminal (esc "~[0m"))
  (unless (eq (terminal-cursor-shape terminal) :default)
    (emit terminal (esc "~[0 q"))
    (setf (terminal-cursor-shape terminal) :default))
  (emit terminal (esc "~[?25h~[?1004l~[?2004l~[?1049l"))
  (setf (terminal-cursor-visible terminal) t))

(defun enter (terminal)
  (enter-raw terminal)
  (write-enter-sequences terminal)
  (flush-output terminal)
  (setf (terminal-state terminal) :entered))

(defun leave (terminal state)
  ;; Restore termios even when writing the escape sequences fails.
  (unwind-protect
       (progn
         (reset-output (terminal-output terminal))
         (write-leave-sequences terminal)
         (flush-output terminal))
    (setf (terminal-state terminal) state)
    (leave-raw terminal)))

(defun terminal-entered-p (terminal)
  "True while TERMINAL is inside WITH-TERMINAL and not suspended."
  (eq (terminal-state terminal) :entered))

(define-condition terminal-already-entered (error)
  ((terminal :initarg :terminal :reader terminal-already-entered-terminal))
  (:report "The terminal is already inside WITH-TERMINAL.")
  (:documentation "Signaled when WITH-TERMINAL is nested on the same terminal."))

(defun call-with-terminal (terminal function &key mouse (kitty-keyboard t))
  "Call FUNCTION with TERMINAL set up for a full-screen application, then restore the
terminal however FUNCTION exits. Setup: raw mode (saving the termios settings), the
alternate screen, a hidden cursor, bracketed paste, focus events, SGR mouse reporting when
MOUSE is true, and, when KITTY-KEYBOARD is true and the terminal supports it (see
tui/caps), the kitty keyboard protocol, pushed on entry or as soon as detection finds it
and popped on exit."
  (unless (eq (terminal-state terminal) :idle)
    (error 'terminal-already-entered :terminal terminal))
  (setf (terminal-mouse terminal) (and mouse t)
        (terminal-kitty-keyboard-wanted terminal) (and kitty-keyboard t))
  (unwind-protect
       (progn
         (enter terminal)
         (funcall function))
    (unless (eq (terminal-state terminal) :idle)
      (leave terminal :idle))))

(defmacro with-terminal ((terminal &rest options &key mouse kitty-keyboard) &body body)
  "Evaluate BODY with TERMINAL set up for a full-screen application and restore it
afterwards, even on a non-local exit. See CALL-WITH-TERMINAL for the OPTIONS."
  (declare (ignore mouse kitty-keyboard))
  `(call-with-terminal ,terminal (lambda () ,@body) ,@options))

(defun suspend (terminal)
  "Restore TERMINAL to how it was before WITH-TERMINAL, for example before stopping the
process on ctrl-z or running a shell command in the terminal. RESUME undoes this."
  (when (eq (terminal-state terminal) :entered)
    (leave terminal :suspended))
  terminal)

(defun resume (terminal)
  "Set TERMINAL up again after SUSPEND. The next frame redraws the whole screen."
  (when (eq (terminal-state terminal) :suspended)
    (enter terminal))
  terminal)

(defun invalidate (terminal)
  "Make the next frame drawn on TERMINAL redraw the whole screen."
  (setf (terminal-invalid terminal) t)
  terminal)

(defun terminal-invalid-p (terminal)
  "True when the next frame must redraw the whole screen."
  (terminal-invalid terminal))

(defun (setf terminal-invalid-p) (value terminal)
  (setf (terminal-invalid terminal) (and value t)))

(defun terminal-cursor-visible-p (terminal)
  "True when TERMINAL's cursor was last left visible."
  (terminal-cursor-visible terminal))

(defun (setf terminal-cursor-visible-p) (value terminal)
  (setf (terminal-cursor-visible terminal) (and value t)))

;;; Features

(defun kitty-keyboard-supported-p (terminal)
  "True when feature detection found that TERMINAL supports the kitty keyboard protocol."
  (terminal-kitty-keyboard-supported terminal))

(defun synchronized-output-supported-p (terminal)
  "True when feature detection found that TERMINAL supports synchronized output."
  (terminal-synchronized-output-supported terminal))

(defun features-detected-p (terminal)
  "True once feature detection has finished on TERMINAL."
  (terminal-features-detected terminal))

(defun record-features (terminal &key kitty-keyboard synchronized-output)
  "Store the results of feature detection in TERMINAL. If TERMINAL is inside a
WITH-TERMINAL that asked for the kitty keyboard protocol and it is now known to be
supported, the protocol is pushed; the sequence is left in the output buffer."
  (setf (terminal-features-detected terminal) t
        (terminal-kitty-keyboard-supported terminal) (and kitty-keyboard t)
        (terminal-synchronized-output-supported terminal) (and synchronized-output t))
  (when (and (eq (terminal-state terminal) :entered)
             (terminal-kitty-keyboard-wanted terminal)
             kitty-keyboard)
    (push-kitty-keyboard terminal))
  terminal)

;;; Input helpers for simple applications

(defun wait-for-input (terminal timeout)
  "Wait until TERMINAL has input or TIMEOUT seconds pass (NIL waits indefinitely). Return
true when input is available. A signal (such as SIGWINCH) may end the wait early.
Applications with their own event loop don't need this."
  (wait-readable (terminal-input-fd terminal) timeout))

(defun read-input (terminal octets &key (start 0) (end (length octets)))
  "Read the input available on TERMINAL into OCTETS from START, at most END - START bytes,
blocking if there is none. Return the number of bytes read."
  (read-octets (terminal-input-fd terminal) octets :start start :end end))
