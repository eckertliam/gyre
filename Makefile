# Build, test and run the systems in this repository with SBCL alone.
#
# ASDF is configured on the command line for each invocation: it sees only the
# .asd files in this repository (plus SBCL's contribs) and writes compiled files
# under build/. No global ASDF, Quicklisp or init file is used. The .claude
# directory is excluded because tools keep git worktrees (copies of the
# repository, with their own .asd files) there.

SBCL ?= sbcl
ROOT := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
BUILD := $(ROOT)/build

LISP := $(SBCL) --noinform --non-interactive --no-sysinit --no-userinit \
	--eval '(require :asdf)' \
	--eval '(asdf:initialize-source-registry (quote (:source-registry (:also-exclude ".claude") (:tree "$(ROOT)/") :ignore-inherited-configuration)))' \
	--eval '(asdf:initialize-output-translations (quote (:output-translations (t ("$(BUILD)/fasl/" :implementation :**/ :*.*.*)) :ignore-inherited-configuration)))' \
	--eval '(setf *compile-verbose* nil *compile-print* nil)'

# Defines (load-strictly SYSTEM): load SYSTEM and exit non-zero if compiling or
# loading it signals any warning, style warnings included, except those SBCL
# muffles itself (such as a macro redefined by loading the fasl it was compiled into).
STRICT := --eval '(defun load-strictly (system) \
	  (let ((count 0)) \
	    (handler-bind ((warning (lambda (c) \
	                              (unless (typep c sb-ext:*muffled-warnings*) \
	                                (incf count) \
	                                (format *error-output* "~&;; ~(~a~): ~a~%" (type-of c) c))))) \
	      (asdf:load-system system)) \
	    (unless (zerop count) \
	      (format *error-output* "~&~d warning(s) while building ~a~%" count system) \
	      (uiop:quit 1))))'

.PHONY: test bench inspector demo clean

# Compile everything from scratch so that every warning is seen, then run the tests.
test:
	rm -rf $(BUILD)/fasl
	$(LISP) $(STRICT) --eval '(load-strictly "tui/tests")' --eval '(tui/tests/harness:main)'

bench:
	$(LISP) $(STRICT) --eval '(load-strictly "tui/tests")' --eval '(tui/tests/bench:main)'

inspector:
	$(LISP) $(STRICT) --eval '(load-strictly "tui/examples")' --eval '(tui/examples/inspector:main)'

demo:
	$(LISP) $(STRICT) --eval '(load-strictly "tui/examples")' --eval '(tui/examples/demo:main)'

clean:
	rm -rf $(BUILD)
