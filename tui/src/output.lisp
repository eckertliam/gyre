;;;; output.lisp — a reusable byte buffer for terminal output.
;;;;
;;;; Escape sequences and text are encoded straight into the buffer as bytes, with no
;;;; FORMAT, streams or intermediate strings, so building a frame doesn't allocate once the
;;;; buffer has grown to the size of a typical frame.

(defpackage #:tui/output
  (:use #:cl)
  (:export #:output
           #:make-output
           #:output-octets
           #:output-fill
           #:reset-output
           #:output-byte
           #:output-ascii
           #:output-decimal
           #:output-char
           #:output-string
           #:output-csi
           #:output-contents))

(in-package #:tui/output)

(defstruct (output (:constructor make-output
                       (&optional (capacity 4096)
                        &aux (octets (make-array capacity
                                                 :element-type '(unsigned-byte 8))))))
  "Bytes waiting to be written to a terminal. OCTETS holds FILL bytes of output; it grows
as needed and is reused after RESET-OUTPUT."
  (octets (make-array 0 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (*)))
  (fill 0 :type fixnum))

(declaim (inline reset-output))
(defun reset-output (output)
  "Discard OUTPUT's contents, keeping its storage."
  (setf (output-fill output) 0)
  output)

(defun grow (output needed)
  (declare (type output output) (type fixnum needed))
  (let* ((old (output-octets output))
         (new (make-array (max needed (* 2 (length old))) :element-type '(unsigned-byte 8))))
    (replace new old :end2 (output-fill output))
    (setf (output-octets output) new)))

(declaim (inline reserve))
(defun reserve (output count)
  "Make room for COUNT more bytes and return the index where they go."
  (declare (type output output) (type fixnum count))
  (let ((fill (output-fill output)))
    (when (> (+ fill count) (length (output-octets output)))
      (grow output (+ fill count)))
    (setf (output-fill output) (+ fill count))
    fill))

(declaim (inline output-byte))
(defun output-byte (output byte)
  "Append the byte BYTE to OUTPUT."
  (declare (type (unsigned-byte 8) byte))
  (let ((index (reserve output 1)))
    (setf (aref (output-octets output) index) byte)))

(defun output-ascii (output string)
  "Append STRING, which must contain only ASCII characters, to OUTPUT."
  (declare (type simple-string string))
  (let ((index (reserve output (length string)))
        (octets (output-octets output)))
    (dotimes (i (length string))
      (setf (aref octets (+ index i)) (char-code (schar string i))))))

(declaim (inline output-csi))
(defun output-csi (output)
  "Append the control sequence introducer ESC [ to OUTPUT."
  (output-byte output 27)
  (output-byte output 91))

(declaim (ftype (function (output (and fixnum unsigned-byte)) (values &optional))
                output-decimal))
(defun output-decimal (output number)
  "Append the decimal digits of the non-negative fixnum NUMBER to OUTPUT."
  (declare (optimize speed))
  (let ((digits (if (zerop number)
                    1
                    (loop for n of-type fixnum = number then (floor n 10)
                          while (plusp n)
                          count t))))
    (declare (type fixnum digits))
    (let ((index (reserve output digits))
          (octets (output-octets output)))
      (loop for position of-type fixnum downfrom (+ index digits -1) to index
            for n of-type fixnum = number then (floor n 10)
            do (setf (aref octets position) (+ 48 (mod n 10))))))
  (values))

(declaim (ftype (function (output character) (values &optional)) output-char))
(defun output-char (output char)
  "Append CHAR to OUTPUT encoded as UTF-8. A surrogate code point becomes U+FFFD."
  (declare (optimize speed))
  (let ((code (char-code char)))
    (when (<= #xD800 code #xDFFF)
      (setf code #xFFFD))
    (cond ((< code #x80)
           (output-byte output code))
          ((< code #x800)
           (let ((i (reserve output 2)) (octets (output-octets output)))
             (setf (aref octets i) (logior #xC0 (ash code -6))
                   (aref octets (+ i 1)) (logior #x80 (logand code #x3F)))))
          ((< code #x10000)
           (let ((i (reserve output 3)) (octets (output-octets output)))
             (setf (aref octets i) (logior #xE0 (ash code -12))
                   (aref octets (+ i 1)) (logior #x80 (logand (ash code -6) #x3F))
                   (aref octets (+ i 2)) (logior #x80 (logand code #x3F)))))
          (t
           (let ((i (reserve output 4)) (octets (output-octets output)))
             (setf (aref octets i) (logior #xF0 (ash code -18))
                   (aref octets (+ i 1)) (logior #x80 (logand (ash code -12) #x3F))
                   (aref octets (+ i 2)) (logior #x80 (logand (ash code -6) #x3F))
                   (aref octets (+ i 3)) (logior #x80 (logand code #x3F)))))))
  (values))

(defun output-string (output string &key (start 0) end)
  "Append STRING from START to END to OUTPUT encoded as UTF-8."
  (declare (type simple-string string) (type fixnum start))
  (loop for i of-type fixnum from start below (or end (length string))
        do (output-char output (schar string i))))

(defun output-contents (output)
  "Return a fresh vector of OUTPUT's bytes. For tests and debugging."
  (subseq (output-octets output) 0 (output-fill output)))
