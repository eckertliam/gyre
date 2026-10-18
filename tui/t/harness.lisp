;;;; harness.lisp — a minimal test harness: deftest, is, signals and a runner.

(defpackage #:tui/tests/harness
  (:use #:cl)
  (:export #:deftest
           #:is
           #:signals
           #:run-tests
           #:main))

(in-package #:tui/tests/harness)

;;; Registry

(defvar *tests* (make-hash-table :test 'eq)
  "Test functions keyed by test name.")

(defvar *test-names* (make-array 0 :adjustable t :fill-pointer t)
  "Test names in the order they were first defined.")

(defun register-test (name function)
  (unless (gethash name *tests*)
    (vector-push-extend name *test-names*))
  (setf (gethash name *tests*) function)
  name)

(defmacro deftest (name &body body)
  "Define a test called NAME whose BODY makes assertions with IS and SIGNALS.
Redefining a test replaces it."
  `(register-test ',name (lambda () ,@body)))

;;; Assertions

(defvar *failures* nil
  "Failure descriptions for the test that is running, newest first.")

(defvar *assertion-count* 0)

(defun record (passed description)
  (incf *assertion-count*)
  (unless passed
    (push description *failures*))
  passed)

(defun call-with-is (form thunk args-thunk)
  ;; THUNK returns the result; ARGS-THUNK, when non-nil, returns the evaluated
  ;; arguments of a function-call form so that failures can show them.
  (multiple-value-bind (result args) (if args-thunk
                                         (let ((args (funcall args-thunk)))
                                           (values (apply thunk args) args))
                                         (values (funcall thunk) nil))
    (record result
            (if args-thunk
                (format nil "~S~%    with arguments ~{~S~^, ~}" form args)
                (format nil "~S" form)))))

(defun function-call-form-p (form)
  (and (consp form)
       (symbolp (first form))
       (fboundp (first form))
       (not (macro-function (first form)))
       (not (special-operator-p (first form)))))

(defmacro is (form)
  "Assert that FORM returns true. When FORM is a function call, its arguments
are evaluated once and shown if the assertion fails."
  (if (function-call-form-p form)
      (let ((args (gensym "ARGS")))
        `(call-with-is ',form
                       (lambda (&rest ,args) (apply #',(first form) ,args))
                       (lambda () (list ,@(rest form)))))
      `(call-with-is ',form (lambda () ,form) nil)))

(defun call-with-signals (condition-type form thunk)
  (let ((signaled (handler-case (progn (funcall thunk) nil)
                    (condition (c)
                      (if (typep c condition-type) t c)))))
    (record (eq signaled t)
            (format nil "~S~%    expected to signal ~S, ~:[returned normally~;~:*signaled ~S~]"
                    form condition-type (and (not (eq signaled t)) signaled)))))

(defmacro signals (condition-type form)
  "Assert that evaluating FORM signals a condition of CONDITION-TYPE."
  `(call-with-signals ',condition-type ',form (lambda () ,form)))

;;; Runner

(defun run-test (name stream debug)
  "Run the test NAME. Return true when it passes."
  (let ((*failures* nil))
    (if debug
        (funcall (gethash name *tests*))
        (handler-case (funcall (gethash name *tests*))
          (error (e)
            (push (format nil "unhandled error: ~A" e) *failures*))))
    (when *failures*
      (format stream "~&FAIL ~S~%" name)
      (dolist (failure (reverse *failures*))
        (format stream "  ~A~%" failure)))
    (null *failures*)))

(defun run-tests (&key package (stream *standard-output*) debug)
  "Run every registered test, or only those whose names are in PACKAGE.
With DEBUG, errors inside tests enter the debugger instead of being recorded.
Print a summary to STREAM and return true when every test passes."
  (let ((*assertion-count* 0)
        (package (and package (find-package package)))
        (run 0)
        (failed 0)
        (start (get-internal-real-time)))
    (loop for name across *test-names*
          when (or (null package) (eq (symbol-package name) package))
            do (incf run)
               (unless (run-test name stream debug)
                 (incf failed)))
    (format stream "~&~D test~:P, ~D assertion~:P, ~D failure~:P in ~,2Fs~%"
            run *assertion-count* failed
            (/ (- (get-internal-real-time) start) internal-time-units-per-second))
    (zerop failed)))

(defun main ()
  "Run every test and exit with status 0 if they all pass, 1 otherwise."
  (uiop:quit (if (run-tests) 0 1)))
