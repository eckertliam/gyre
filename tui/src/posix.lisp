;;;; posix.lisp — the system calls tui needs on a terminal file descriptor.
;;;;
;;;; Every function takes the descriptor explicitly; nothing here opens /dev/tty or touches
;;;; the process's standard streams.

(defpackage #:tui/posix
  (:use #:cl)
  (:import-from #:sb-posix)
  (:import-from #:sb-alien
                #:alien-funcall #:extern-alien #:with-alien #:slot #:addr
                #:int #:short #:unsigned-short #:unsigned-long #:unsigned-int #:struct
                #:get-errno)
  (:export #:system-call-failed
           #:system-call-failed-name
           #:system-call-failed-errno
           #:enter-raw-mode
           #:restore-termios
           #:raw-mode-p
           #:window-size
           #:wait-readable
           #:read-octets
           #:write-octets))

(in-package #:tui/posix)

(define-condition system-call-failed (error)
  ((name :initarg :name :reader system-call-failed-name)
   (errno :initarg :errno :reader system-call-failed-errno))
  (:report (lambda (condition stream)
             (format stream "~A failed with errno ~D."
                     (system-call-failed-name condition)
                     (system-call-failed-errno condition))))
  (:documentation "Signaled when a system call on a terminal descriptor fails."))

(defun fail (name)
  (error 'system-call-failed :name name :errno (get-errno)))

;;; Raw mode

(defun make-raw (termios)
  ;; The same changes as cfmakeraw(3).
  (setf (sb-posix:termios-iflag termios)
        (logandc2 (sb-posix:termios-iflag termios)
                  (logior sb-posix:ignbrk sb-posix:brkint sb-posix:parmrk sb-posix:istrip
                          sb-posix:inlcr sb-posix:igncr sb-posix:icrnl sb-posix:ixon))
        (sb-posix:termios-oflag termios)
        (logandc2 (sb-posix:termios-oflag termios) sb-posix:opost)
        (sb-posix:termios-lflag termios)
        (logandc2 (sb-posix:termios-lflag termios)
                  (logior sb-posix:echo sb-posix:echonl sb-posix:icanon sb-posix:isig
                          sb-posix:iexten))
        (sb-posix:termios-cflag termios)
        (logior (logandc2 (sb-posix:termios-cflag termios)
                          (logior sb-posix:csize sb-posix:parenb))
                sb-posix:cs8))
  (let ((cc (sb-posix:termios-cc termios)))
    (setf (aref cc sb-posix:vmin) 1
          (aref cc sb-posix:vtime) 0))
  termios)

(defun enter-raw-mode (fd)
  "Put the terminal on FD into raw mode and return its previous settings, for
RESTORE-TERMIOS. Pending input is kept."
  (let ((original (sb-posix:tcgetattr fd)))
    (sb-posix:tcsetattr fd sb-posix:tcsanow (make-raw (sb-posix:tcgetattr fd)))
    original))

(defun restore-termios (fd termios)
  "Give the terminal on FD the settings TERMIOS, as returned by ENTER-RAW-MODE."
  (sb-posix:tcsetattr fd sb-posix:tcsanow termios))

(defun raw-mode-p (fd)
  "True when the terminal on FD has canonical mode and echo turned off."
  (let ((lflag (sb-posix:termios-lflag (sb-posix:tcgetattr fd))))
    (not (logtest lflag (logior sb-posix:icanon sb-posix:echo)))))

;;; Window size

(sb-alien:define-alien-type nil
  (struct winsize
    (rows unsigned-short)
    (columns unsigned-short)
    (x-pixels unsigned-short)
    (y-pixels unsigned-short)))

(defconstant +tiocgwinsz+ #+darwin #x40087468 #+linux #x5413)

(declaim (inline %ioctl-winsize))
(defun %ioctl-winsize (fd winsize)
  ;; ioctl is variadic: &OPTIONAL marks the variadic argument for the foreign call.
  (alien-funcall (extern-alien "ioctl" (function int int unsigned-long &optional
                                                 (* (struct winsize))))
                 fd +tiocgwinsz+ winsize))

(defun window-size (fd)
  "Return the size of the terminal on FD as two values, columns and rows, or NIL when FD
is not a terminal or reports a zero size."
  (declare (type fixnum fd))
  (with-alien ((winsize (struct winsize)))
    (if (and (zerop (%ioctl-winsize fd (addr winsize)))
             (plusp (slot winsize 'columns))
             (plusp (slot winsize 'rows)))
        (values (slot winsize 'columns) (slot winsize 'rows))
        nil)))

;;; Waiting, reading and writing

(sb-alien:define-alien-type nil
  (struct pollfd
    (fd int)
    (events short)
    (revents short)))

(defconstant +pollin+ 1)
(defconstant +pollout+ 4)

(defun poll-one (fd events timeout-ms)
  (declare (type fixnum fd timeout-ms))
  (with-alien ((pfd (struct pollfd)))
    (setf (slot pfd 'fd) fd
          (slot pfd 'events) events
          (slot pfd 'revents) 0)
    (let ((result (alien-funcall (extern-alien "poll"
                                               (function int (* (struct pollfd))
                                                         #+darwin unsigned-int
                                                         #+linux unsigned-long
                                                         int))
                                 (addr pfd) 1 timeout-ms)))
      (cond ((plusp result) t)
            ((zerop result) nil)
            ((= (get-errno) sb-posix:eintr) nil)
            (t (fail "poll"))))))

(defun wait-readable (fd timeout)
  "Wait until FD has input to read or TIMEOUT seconds pass (NIL waits indefinitely).
Return true when input is available. A signal interrupting the wait returns NIL early."
  (poll-one fd +pollin+ (if timeout (max 0 (round (* timeout 1000))) -1)))

(defun read-octets (fd octets &key (start 0) (end (length octets)))
  "Read whatever input is available on FD, up to END - START bytes, into OCTETS at START.
Blocks if none is available. Return the number of bytes read: 0 at end of file or when a
signal interrupted the read."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets) (type fixnum fd start end))
  (let ((count (sb-sys:with-pinned-objects (octets)
                 (alien-funcall (extern-alien "read" (function sb-alien:long int
                                                               sb-alien:system-area-pointer
                                                               unsigned-long))
                                fd (sb-sys:sap+ (sb-sys:vector-sap octets) start)
                                (- end start)))))
    (cond ((>= count 0) count)
          ((let ((errno (get-errno)))
             (or (= errno sb-posix:eintr) (= errno sb-posix:eagain)))
           0)
          (t (fail "read")))))

(declaim (ftype (function (fixnum (simple-array (unsigned-byte 8) (*)) fixnum fixnum)
                          (values fixnum &optional))
                %write))
(defun %write (fd octets start end)
  (sb-sys:with-pinned-objects (octets)
    (alien-funcall (extern-alien "write" (function sb-alien:long int
                                                   sb-alien:system-area-pointer
                                                   unsigned-long))
                   fd (sb-sys:sap+ (sb-sys:vector-sap octets) start) (- end start))))

(defun write-octets (fd octets &key (start 0) (end (length octets)))
  "Write OCTETS from START to END to FD, retrying after partial writes, interruptions and,
for a non-blocking FD, waiting until it is writable. Normally a single write(2)."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets) (type fixnum fd start end))
  (loop while (< start end)
        do (let ((count (%write fd octets start end)))
             (cond ((>= count 0) (incf start count))
                   ((= (get-errno) sb-posix:eintr))
                   ((= (get-errno) sb-posix:eagain) (poll-one fd +pollout+ -1))
                   (t (fail "write"))))))
