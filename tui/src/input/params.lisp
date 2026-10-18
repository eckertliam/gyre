;;;; params.lisp — the parameters of a CSI sequence, parsed byte by byte as they arrive.
;;;;
;;;; Parameters are fields separated by semicolons, each holding one or more values
;;;; separated by colons: in CSI 97:65;2u, field 0 holds 97 and 65 and field 1
;;;; holds 2. Values are parsed straight from the bytes into a reused array.

(defpackage #:tui/input/params
  (:use #:cl)
  (:export #:params #:make-params #:params-reset #:params-feed #:params-finish
           #:param #:param-count #:param-field-count #:params-marker #:params-intermediate
           #:params-odd #:params-empty-p))

(in-package #:tui/input/params)

(defconstant +capacity+ 256
  "Most values one sequence can hold. A CSI sequence is never longer than this.")

(defconstant +max-value+ 1000000000
  "Values above this are clamped to it, so that arithmetic stays in fixnums.")

(deftype value-vector () '(simple-array fixnum (*)))

(defstruct (params (:constructor make-params ()) (:copier nil))
  "The parameters of the CSI sequence being decoded. VALUES holds every value in
order, -1 for an empty one; field I's values start at index (aref STARTS I) and
end before (aref STARTS (1+ I)). MARKER is the private marker byte (one of
< = > ?) that began the parameters, or 0. INTERMEDIATE is the intermediate byte
before the final byte, or 0. ODD is true when the parameters are well-formed but
not something this decoder interprets (a misplaced marker, several
intermediates, too many values)."
  (values (make-array +capacity+ :element-type 'fixnum :initial-element -1)
   :type value-vector)
  (starts (make-array (1+ +capacity+) :element-type 'fixnum :initial-element 0)
   :type value-vector)
  (count 0 :type fixnum)
  (field-count 0 :type fixnum)
  (current -1 :type (integer -1 1000000000))
  (marker 0 :type (unsigned-byte 8))
  (intermediate 0 :type (unsigned-byte 8))
  (empty t :type boolean)
  (odd nil :type boolean))

(declaim (inline params-empty-p))
(defun params-empty-p (params)
  "True when the sequence had no parameter bytes besides its private marker."
  (params-empty params))

(defun params-reset (params)
  "Prepare PARAMS for a new sequence."
  (declare (type params params))
  (setf (params-count params) 0
        (params-field-count params) 0
        (aref (params-starts params) 0) 0
        (params-current params) -1
        (params-marker params) 0
        (params-intermediate params) 0
        (params-empty params) t
        (params-odd params) nil)
  params)

(defun push-value (params)
  (declare (optimize speed) (type params params))
  (let ((count (params-count params)))
    (if (< count +capacity+)
        (setf (aref (params-values params) count) (params-current params)
              (params-count params) (1+ count))
        (setf (params-odd params) t))
    (setf (params-current params) -1)))

(defun end-field (params)
  (declare (optimize speed) (type params params))
  (push-value params)
  (unless (params-odd params)
    (let ((fields (1+ (params-field-count params))))
      (setf (params-field-count params) fields
            (aref (params-starts params) fields) (params-count params)))))

(defun add-digit (params digit)
  (declare (optimize speed) (type params params) (type (integer 0 9) digit))
  (let ((current (params-current params)))
    (setf (params-current params)
          (if (< current 0)
              digit
              (min (+ (* current 10) digit) +max-value+)))))

(defun params-feed (params byte)
  "Add the parameter or intermediate byte BYTE (#x20 to #x3F) to PARAMS. Return
false when BYTE can't appear at this point of a CSI sequence."
  (declare (optimize speed) (type params params) (type (unsigned-byte 8) byte))
  (cond ((/= 0 (params-intermediate params))
         ;; Only further intermediates may follow an intermediate.
         (when (<= #x20 byte #x2f)
           (setf (params-odd params) t)))
        ((<= #x20 byte #x2f) (setf (params-intermediate params) byte) t)
        ((<= #x3c byte #x3f)
         (if (and (params-empty params) (= 0 (params-marker params)))
             (setf (params-marker params) byte)
             (setf (params-odd params) t))
         t)
        ((<= #x30 byte #x3b)
         (setf (params-empty params) nil)
         (case byte
           (#x3a (push-value params))
           (#x3b (end-field params))
           (t (add-digit params (- byte #x30))))
         t)))

(defun params-finish (params)
  "Finish the parameters when the final byte arrives."
  (declare (type params params))
  (end-field params))

(declaim (inline param-field-count))
(defun param-field-count (params)
  "The number of fields in the finished PARAMS."
  (params-field-count params))

(defun param (params field &optional (sub 0) (default -1))
  "Return value SUB of field FIELD in the finished PARAMS, or DEFAULT when that
value is missing or empty."
  (declare (type params params) (type fixnum field sub default))
  (if (< field (params-field-count params))
      (let ((index (+ (aref (params-starts params) field) sub)))
        (if (< index (aref (params-starts params) (1+ field)))
            (let ((value (aref (params-values params) index)))
              (if (< value 0) default value))
            default))
      default))

(defun param-count (params field)
  "The number of values in field FIELD of the finished PARAMS, 0 when absent."
  (declare (type params params) (type fixnum field))
  (if (< field (params-field-count params))
      (- (aref (params-starts params) (1+ field)) (aref (params-starts params) field))
      0))
