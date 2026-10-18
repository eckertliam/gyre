;;;; table-state.lisp — the state of a table widget between frames: the selected row and the
;;;; scroll offset, as ratatui's TableState.

(defpackage #:tui/widgets/table-state
  (:use #:cl)
  (:export #:table-state
           #:table-state-p
           #:make-table-state
           #:table-state-selected
           #:table-state-offset
           #:table-select
           #:table-select-next
           #:table-select-previous
           #:table-select-first
           #:table-select-last))

(in-package #:tui/widgets/table-state)

(declaim (inline make-table-state))
(defstruct (table-state (:constructor make-table-state (&key selected (offset 0)))
                        (:copier nil))
  "What a table widget remembers between frames. SELECTED is the index of the selected row,
or NIL. OFFSET is the index of the first row shown; rendering adjusts it to keep the
selected row in view. An index past the last row is clamped when the table is rendered, and
rendering a table without rows clears the selection, as in ratatui."
  (selected nil :type (or null (and fixnum unsigned-byte)))
  (offset 0 :type (and fixnum unsigned-byte)))

(defun table-select (state index)
  "Select row INDEX of a table with STATE, or nothing when INDEX is NIL, which also scrolls
back to the top."
  (check-type index (or null (and fixnum unsigned-byte)))
  (setf (table-state-selected state) index)
  (unless index
    (setf (table-state-offset state) 0))
  state)

(defun table-select-next (state)
  "Select the row after the selected one, or the first when none is selected."
  (let ((selected (table-state-selected state)))
    (table-select state (if selected (min (1+ selected) most-positive-fixnum) 0))))

(defun table-select-previous (state)
  "Select the row before the selected one (staying at the first), or the last when none is
selected."
  (let ((selected (table-state-selected state)))
    (table-select state (if selected (max (1- selected) 0) most-positive-fixnum))))

(defun table-select-first (state)
  "Select the first row."
  (table-select state 0))

(defun table-select-last (state)
  "Select the last row. Until the table is rendered, the index is MOST-POSITIVE-FIXNUM."
  (table-select state most-positive-fixnum))
