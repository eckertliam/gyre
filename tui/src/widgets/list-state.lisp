;;;; list-state.lisp — the state of a list widget between frames: the selected item and the
;;;; scroll offset, as ratatui's ListState.

(defpackage #:tui/widgets/list-state
  (:use #:cl)
  (:export #:list-state
           #:list-state-p
           #:make-list-state
           #:list-state-selected
           #:list-state-offset
           #:list-select
           #:list-select-next
           #:list-select-previous
           #:list-select-first
           #:list-select-last))

(in-package #:tui/widgets/list-state)

(declaim (inline make-list-state))
(defstruct (list-state (:constructor make-list-state (&key selected (offset 0)))
                       (:copier nil))
  "What a list widget remembers between frames. SELECTED is the index of the selected item,
or NIL. OFFSET is the index of the first item shown; rendering adjusts it to keep the
selected item in view. An index past the last item is clamped when the list is rendered, and
rendering an empty list clears the selection, as in ratatui."
  (selected nil :type (or null (and fixnum unsigned-byte)))
  (offset 0 :type (and fixnum unsigned-byte)))

(defun list-select (state index)
  "Select item INDEX of a list with STATE, or nothing when INDEX is NIL, which also scrolls
back to the top."
  (check-type index (or null (and fixnum unsigned-byte)))
  (setf (list-state-selected state) index)
  (unless index
    (setf (list-state-offset state) 0))
  state)

(defun list-select-next (state)
  "Select the item after the selected one, or the first when none is selected."
  (let ((selected (list-state-selected state)))
    (list-select state (if selected (min (1+ selected) most-positive-fixnum) 0))))

(defun list-select-previous (state)
  "Select the item before the selected one (staying at the first), or the last when none is
selected."
  (let ((selected (list-state-selected state)))
    (list-select state (if selected (max (1- selected) 0) most-positive-fixnum))))

(defun list-select-first (state)
  "Select the first item."
  (list-select state 0))

(defun list-select-last (state)
  "Select the last item. Until the list is rendered, the index is MOST-POSITIVE-FIXNUM."
  (list-select state most-positive-fixnum))
