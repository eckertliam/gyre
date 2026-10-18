;;;; table-row.lisp — the rows of a table widget: cells, height, margins and style, as
;;;; ratatui's Row.

(defpackage #:tui/widgets/table-row
  (:use #:cl)
  (:import-from #:tui/style #:style)
  (:import-from #:tui/widgets/draw #:as-text)
  (:export #:table-row
           #:table-row-p
           #:table-row-cells
           #:table-row-height
           #:table-row-top-margin
           #:table-row-bottom-margin
           #:table-row-style))

(in-package #:tui/widgets/table-row)

(defstruct (table-row (:constructor %make-table-row
                          (cells height top-margin bottom-margin style))
                      (:copier nil))
  "A row of a table: CELLS, a simple vector of texts drawn in the table's columns, HEIGHT rows
tall with TOP-MARGIN and BOTTOM-MARGIN empty rows around it. STYLE (or NIL) patches the row
under its cells."
  (cells #() :type simple-vector :read-only t)
  (height 1 :type (and fixnum unsigned-byte) :read-only t)
  (top-margin 0 :type (and fixnum unsigned-byte) :read-only t)
  (bottom-margin 0 :type (and fixnum unsigned-byte) :read-only t)
  (style nil :type (or null style) :read-only t))

(deftype cells-designator ()
  '(or cl:list (and vector (not string))))

(defun table-row (cells &key (height 1) (top-margin 0) (bottom-margin 0) style)
  "Return a table row. CELLS is a list or vector of cell designators, each a text designator
(see MAKE-TEXT). The row is HEIGHT rows tall (1 by default, whatever its cells hold) with
TOP-MARGIN and BOTTOM-MARGIN empty rows above and below it. STYLE, a style or NIL, patches
the row under its cells."
  (check-type cells cells-designator)
  (check-type height (and fixnum unsigned-byte))
  (check-type top-margin (and fixnum unsigned-byte))
  (check-type bottom-margin (and fixnum unsigned-byte))
  (check-type style (or null style))
  (%make-table-row (map 'simple-vector #'as-text cells) height top-margin bottom-margin
                   style))
