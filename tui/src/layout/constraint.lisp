;;;; constraint.lisp — layout constraints: designator lists such as (:length 5) and the
;;;; constraint structs they are parsed into.

(defpackage #:tui/layout/constraint
  (:use #:cl)
  (:export #:constraint
           #:constraint-kind
           #:constraint-value
           #:constraint-denominator
           #:parse-constraint
           #:invalid-constraint
           #:invalid-constraint-designator))

(in-package #:tui/layout/constraint)

(define-condition invalid-constraint (error)
  ((designator :initarg :designator :reader invalid-constraint-designator))
  (:report (lambda (condition stream)
             (format stream "Invalid layout constraint ~S: expected (:length n), (:min n), ~
                             (:max n), (:percentage n), (:ratio n d) or (:fill [n]), where ~
                             each n is a non-negative fixnum and d a positive one."
                     (invalid-constraint-designator condition))))
  (:documentation "Signaled when a layout is made with a constraint designator that is not one of
(:length n), (:min n), (:max n), (:percentage n), (:ratio n d) or (:fill [n]) with non-negative
fixnums n and a positive fixnum d. INVALID-CONSTRAINT-DESIGNATOR returns the offending object."))

(defstruct (constraint (:constructor make-constraint (kind value &optional (denominator 1)))
                       (:copier nil))
  "A parsed constraint. KIND is the designator's keyword; VALUE is its number (the numerator
for :ratio, the weight for :fill); DENOMINATOR is the denominator of a :ratio and 1 otherwise."
  (kind :fill :type (member :length :min :max :percentage :ratio :fill) :read-only t)
  (value 0 :type (and fixnum unsigned-byte) :read-only t)
  (denominator 1 :type (and fixnum (integer 1)) :read-only t))

(defun argsp (args count)
  "Return true if ARGS is a list of exactly COUNT non-negative fixnums."
  (loop repeat count
        unless (and (consp args) (typep (first args) '(and fixnum unsigned-byte)))
          return nil
        do (pop args)
        finally (return (null args))))

(defun parse-arguments (head args)
  "Return the constraint for a designator with keyword HEAD and arguments ARGS, or NIL if they
don't form a valid designator."
  (cond ((and (member head '(:length :min :max :percentage)) (argsp args 1))
         (make-constraint head (first args)))
        ((and (eq head :ratio) (argsp args 2) (plusp (second args)))
         (make-constraint :ratio (first args) (second args)))
        ((and (eq head :fill) (null args))
         (make-constraint :fill 1))
        ((and (eq head :fill) (argsp args 1))
         (make-constraint :fill (first args)))))

(defun parse-constraint (designator)
  "Return the constraint described by DESIGNATOR, or signal INVALID-CONSTRAINT."
  (or (and (consp designator) (parse-arguments (first designator) (rest designator)))
      (error 'invalid-constraint :designator designator)))
