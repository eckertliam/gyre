;;;; pty.lisp — pseudo-terminals for tests: open, resize, read what the program wrote.

(defpackage #:tui/tests/pty
  (:use #:cl)
  (:import-from #:sb-alien #:alien-funcall #:extern-alien #:int #:c-string #:with-alien
                #:slot #:addr #:unsigned-short #:unsigned-long #:struct)
  (:export #:pty
           #:open-pty
           #:close-pty
           #:pty-master
           #:pty-slave
           #:set-pty-size
           #:read-pty
           #:write-pty
           #:drain-pty
           #:with-pty
           #:octets
           #:octets-string
           #:search-octets
           #:esc))

(in-package #:tui/tests/pty)

(defstruct (pty (:constructor make-pty (master slave)))
  (master -1 :type fixnum)
  (slave -1 :type fixnum))

(sb-alien:define-alien-type nil
  (struct test-winsize
    (rows unsigned-short)
    (columns unsigned-short)
    (x-pixels unsigned-short)
    (y-pixels unsigned-short)))

(defconstant +tiocswinsz+ #+darwin #x80087467 #+linux #x5414)

(defun check (result name)
  (when (minusp result)
    (error "~A failed with errno ~D" name (sb-alien:get-errno)))
  result)

(defun set-pty-size (pty columns rows)
  "Set the window size of PTY."
  (with-alien ((winsize (struct test-winsize)))
    (setf (slot winsize 'rows) rows
          (slot winsize 'columns) columns
          (slot winsize 'x-pixels) 0
          (slot winsize 'y-pixels) 0)
    (check (alien-funcall (extern-alien "ioctl" (function int int unsigned-long &optional
                                                          (* (struct test-winsize))))
                          (pty-master pty) +tiocswinsz+ (addr winsize))
           "ioctl(TIOCSWINSZ)")))

(defun open-pty (columns rows)
  "Open a new pseudo-terminal of the given size and return it."
  (let* ((master (check (alien-funcall (extern-alien "posix_openpt" (function int int))
                                       (logior sb-posix:o-rdwr sb-posix:o-noctty))
                        "posix_openpt"))
         (name (progn
                 (check (alien-funcall (extern-alien "grantpt" (function int int)) master)
                        "grantpt")
                 (check (alien-funcall (extern-alien "unlockpt" (function int int)) master)
                        "unlockpt")
                 (alien-funcall (extern-alien "ptsname" (function c-string int)) master)))
         (slave (sb-posix:open name (logior sb-posix:o-rdwr sb-posix:o-noctty)))
         (pty (make-pty master slave)))
    (set-pty-size pty columns rows)
    pty))

(defun close-pty (pty)
  (sb-posix:close (pty-slave pty))
  (sb-posix:close (pty-master pty)))

(defmacro with-pty ((var columns rows) &body body)
  `(let ((,var (open-pty ,columns ,rows)))
     (unwind-protect (progn ,@body)
       (close-pty ,var))))

(sb-alien:define-alien-type nil
  (struct pollfd-test (fd int) (events sb-alien:short) (revents sb-alien:short)))

(defun poll-readable (fd timeout-ms)
  (with-alien ((pfd (struct pollfd-test)))
    (setf (slot pfd 'fd) fd (slot pfd 'events) 1 (slot pfd 'revents) 0)
    (plusp (alien-funcall (extern-alien "poll" (function int (* (struct pollfd-test))
                                                         unsigned-long int))
                          (addr pfd) 1 timeout-ms))))

(defun read-pty (pty &key (timeout 0.2))
  "Return everything PTY's program side has written so far, as octets, waiting up to TIMEOUT
seconds for the first byte and then until no more arrives for 20 ms."
  (let ((chunks '())
        (buffer (make-array 65536 :element-type '(unsigned-byte 8))))
    (loop with wait = (round (* timeout 1000))
          while (poll-readable (pty-master pty) wait)
          do (let ((count (sb-sys:with-pinned-objects (buffer)
                            (sb-posix:read (pty-master pty) (sb-sys:vector-sap buffer)
                                           (length buffer)))))
               (when (zerop count) (loop-finish))
               (push (subseq buffer 0 count) chunks)
               (setf wait 20)))
    (let ((result (make-array (reduce #'+ chunks :key #'length)
                              :element-type '(unsigned-byte 8)))
          (position 0))
      (dolist (chunk (reverse chunks) result)
        (replace result chunk :start1 position)
        (incf position (length chunk))))))

(defun drain-pty (pty)
  "Discard whatever PTY's program side has written."
  (read-pty pty :timeout 0))

(defun write-pty (pty octets)
  "Write OCTETS to PTY's master side, so the program side reads them as input."
  (let ((octets (if (stringp octets) (octets octets) octets)))
    (sb-sys:with-pinned-objects (octets)
      (sb-posix:write (pty-master pty) (sb-sys:vector-sap octets) (length octets)))))

(defun esc (string)
  "STRING with each ~ replaced by ESC."
  (substitute (code-char 27) #\~ string))

(defun octets (string)
  "STRING encoded as UTF-8 octets, with each ~ replaced by ESC."
  (sb-ext:string-to-octets (esc string) :external-format :utf-8))

(defun octets-string (octets)
  "OCTETS decoded as UTF-8, with ESC shown as ~."
  (substitute #\~ (code-char 27) (sb-ext:octets-to-string (coerce octets '(vector (unsigned-byte 8)))
                                                          :external-format :utf-8)))

(defun search-octets (needle haystack)
  "True when the octets of the string NEEDLE (with ~ for ESC) occur in HAYSTACK."
  (search (octets needle) haystack))
