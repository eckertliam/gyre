;;;; table.lisp — the table widget: rows of cells in columns sized by layout constraints, with a
;;;; header, a footer and a selected row that scrolls into view, as in ratatui. Its state is a
;;;; TABLE-STATE (tui/widgets/table-state) and its rows are TABLE-ROWs (tui/widgets/table-row).

(defpackage #:tui/widgets/table
  (:use #:cl)
  (:shadowing-import-from #:tui/widgets/block #:block)
  (:import-from #:tui/widgets/block #:block-inner-bounds)
  (:import-from #:tui/style #:style)
  (:import-from #:tui/layout #:make-rect #:rect-x #:rect-width #:make-layout #:split)
  (:import-from #:tui/text #:text-width)
  (:import-from #:tui/widgets/widget #:render #:render-stateful)
  (:import-from #:tui/widgets/draw #:clip-area #:as-text #:highlight-spacing #:selection-column-p)
  (:import-from #:tui/buffer #:set-style-region)
  (:import-from #:tui/buffer/text #:set-text-region)
  (:import-from #:tui/widgets/table-row
                #:table-row #:table-row-p #:table-row-cells #:table-row-height
                #:table-row-top-margin #:table-row-bottom-margin #:table-row-style)
  (:import-from #:tui/widgets/table-state
                #:table-state #:make-table-state #:table-state-selected #:table-state-offset
                #:table-select)
  (:export #:table))

(in-package #:tui/widgets/table)

;;; Column widths

(defstruct (columns (:constructor make-columns (layout)) (:copier nil) (:predicate nil))
  "A table's column layout and the area it last split: the table's width less the highlight
symbol column, positioned relative to the table's left edge. Reusing them keeps rendering at
the same width from allocating. A table keeps one for drawing with the highlight symbol
column and one for drawing without it."
  (layout nil)
  (max-width -1 :type fixnum)
  (selection-width -1 :type fixnum)
  (area nil))

(defclass table ()
  ((rows :initarg :rows :reader table-rows)
   (header :initarg :header :reader table-header)
   (footer :initarg :footer :reader table-footer)
   (widths :initarg :widths :reader table-widths)
   (column-count :initarg :column-count :reader table-column-count)
   (column-spacing :initarg :column-spacing :reader table-column-spacing)
   (flex :initarg :flex :reader table-flex)
   (columns :initarg :columns :reader table-columns)
   (block :initarg :block :reader table-block)
   (style :initarg :style :reader table-style)
   (row-highlight-style :initarg :row-highlight-style :reader table-row-highlight-style)
   (highlight-symbol :initarg :highlight-symbol :reader table-highlight-symbol)
   (symbol-width :initarg :symbol-width :reader table-symbol-width)
   (highlight-spacing :initarg :highlight-spacing :reader table-highlight-spacing)
   (scroll-padding :initarg :scroll-padding :reader table-scroll-padding))
  (:documentation "A widget that shows rows of cells in columns, with an optional header and
footer, one row of which can be selected and highlighted. Its state, a TABLE-STATE, holds the
selection and the scroll offset. Make one with TABLE. A table caches its column positions
for the last width it was drawn at, so it must not be rendered by two threads at once."))

(defun row-designator (designator)
  "The row DESIGNATOR designates: a table row, or a list or vector of cell designators."
  (if (table-row-p designator) designator (table-row designator)))

(defun column-count (rows header footer)
  "The number of columns the cells of ROWS, HEADER and FOOTER (either may be NIL) fill."
  (reduce #'max (concatenate 'cl:list rows (remove nil (cl:list header footer)))
          :key (lambda (row) (length (table-row-cells row))) :initial-value 0))

(defun column-caches (widths flex spacing)
  "The two column caches of a table: without and with a highlight symbol column. Making
them checks WIDTHS, FLEX and SPACING."
  (flet ((make-cache ()
           (let ((layout (make-layout :direction :horizontal :constraints widths :flex flex
                                      :spacing spacing)))
             (make-columns (and widths layout)))))
    (vector (make-cache) (make-cache))))

(defun table (rows &key widths header footer (column-spacing 1) (flex :start) block style
                     row-highlight-style highlight-symbol (highlight-spacing :when-selected)
                     (scroll-padding 0))
  "Return a table widget showing ROWS, a list or vector of row designators: each a TABLE-ROW,
or a list or vector of cell designators, which are text designators (see MAKE-TEXT).

WIDTHS is a list of layout constraint designators, one per column, such as (:length 10),
(:percentage 30), (:min 5) or (:fill 1) (see MAKE-LAYOUT); the columns are laid out like a
horizontal layout with FLEX (default :start) and COLUMN-SPACING (default 1) columns between
them. Without WIDTHS every column gets an equal share, (:length (floor width columns)). Cells
beyond the last column are not drawn.

HEADER and FOOTER, row designators or NIL, are drawn at the top and bottom and don't scroll.
BLOCK, a block or NIL, is drawn around the table. STYLE patches the whole area, under the
rows' and cells' styles. ROW-HIGHLIGHT-STYLE patches the selected row after it is drawn.

HIGHLIGHT-SYMBOL, a text designator or NIL, is drawn in a column left of the selected row.
HIGHLIGHT-SPACING says when that column is reserved: :always, :when-selected (the default:
only while a row is selected) or :never. When a row is selected, rendering scrolls the table
so that it is visible, with SCROLL-PADDING more rows on either side when they fit."
  (check-type rows sequence)
  (check-type widths cl:list)
  (check-type column-spacing (and fixnum unsigned-byte))
  (check-type block (or null block))
  (check-type style (or null style))
  (check-type row-highlight-style (or null style))
  (check-type highlight-spacing highlight-spacing)
  (check-type scroll-padding (and fixnum unsigned-byte))
  (let* ((columns (column-caches widths flex column-spacing))
         (rows (map 'simple-vector #'row-designator rows))
         (header (and header (row-designator header)))
         (footer (and footer (row-designator footer)))
         (symbol (and highlight-symbol (as-text highlight-symbol))))
    (make-instance 'table
                   :rows rows
                   :header header
                   :footer footer
                   :widths widths
                   :column-count (column-count rows header footer)
                   :column-spacing column-spacing
                   :flex flex
                   :columns columns
                   :block block
                   :style style
                   :row-highlight-style row-highlight-style
                   :highlight-symbol symbol
                   :symbol-width (if symbol (text-width symbol) 0)
                   :highlight-spacing highlight-spacing
                   :scroll-padding scroll-padding)))

(defun column-areas (table max-width selection-width)
  "Return a simple vector of rects, one per column, whose X and WIDTH give the columns'
positions relative to the left edge of a table MAX-WIDTH columns wide that reserves
SELECTION-WIDTH columns for the highlight symbol. Allocates only when either width changed
since the last call with or without a highlight symbol column."
  (declare (type fixnum max-width selection-width))
  (let ((columns (svref (table-columns table) (if (plusp selection-width) 1 0))))
    (unless (and (= max-width (columns-max-width columns))
                 (= selection-width (columns-selection-width columns)))
      (let ((start (min selection-width max-width)))
        (setf (columns-max-width columns) max-width
              (columns-selection-width columns) selection-width
              (columns-area columns) (make-rect start 0 (- max-width start) 1))
        (unless (table-widths table)
          (let ((count (table-column-count table)))
            (setf (columns-layout columns)
                  (make-layout :direction :horizontal
                               :constraints (make-list count :initial-element
                                                       `(:length ,(floor max-width
                                                                         (max 1 count))))
                               :flex (table-flex table)
                               :spacing (table-column-spacing table)))))))
    (split (columns-layout columns) (columns-area columns))))

;;; Which rows are visible
;;;
;;; These are ratatui's Table::visible_rows and apply_scroll_padding_to_selected_index.

(defun row-span (row)
  "The rows ROW takes with its margins."
  (+ (table-row-height row) (table-row-top-margin row) (table-row-bottom-margin row)))

(defun padded-selection (rows selected max-height first last padding)
  "The index of the row that has to be visible for row SELECTED to be shown with PADDING rows
around it, given that rows FIRST to LAST (exclusive) are visible now. The padding shrinks
until the padded rows fit in MAX-HEIGHT rows."
  (declare (type simple-vector rows) (type fixnum selected max-height first last padding))
  (let ((last-valid (1- (length rows))))
    (if (zerop padding)
        selected
        (let* ((padding (min padding last-valid))
               (height (loop for i from (max 0 (- selected padding))
                               to (min (+ selected padding) last-valid)
                             sum (row-span (svref rows i)))))
          (declare (type fixnum padding height))
          (loop while (and (plusp padding) (> height max-height))
                do (when (>= selected padding)
                     (decf height (row-span (svref rows (- selected padding)))))
                   (when (<= (+ selected padding) last-valid)
                     (decf height (row-span (svref rows (+ selected padding)))))
                   (decf padding))
          (let ((after (min (+ selected padding) last-valid)))
            (cond ((>= after last) after)
                  ((< (max 0 (- selected padding)) first) (max 0 (- selected padding)))
                  (t selected)))))))

(defun visible-rows (rows selected offset max-height padding)
  "Return the index of the first visible row and of the row after the last one, for ROWS
shown in MAX-HEIGHT rows from row OFFSET, scrolled to show row SELECTED (or NIL) with PADDING
rows around it. The last row may be only partly visible."
  (declare (type simple-vector rows) (type fixnum offset max-height padding))
  (let* ((last-row (max 0 (1- (length rows))))
         (start (min offset last-row (or selected last-row)))
         (end start)
         (height 0))
    (declare (type fixnum start end height))
    (loop for i from start below (length rows)
          for row = (svref rows i)
          while (<= (+ height (table-row-height row)) max-height)
          do (incf height (row-span row))
             (incf end))
    (when selected
      (let ((target (padded-selection rows (min selected last-row) max-height start end
                                      padding)))
        (loop while (>= target end)
              do (incf height (row-span (svref rows end)))
                 (incf end)
                 (loop while (> height max-height)
                       do (setf height (max 0 (- height (row-span (svref rows start)))))
                          (incf start)))
        (loop while (< target start)
              do (decf start)
                 (incf height (row-span (svref rows start)))
                 (loop while (> height max-height)
                       do (decf end)
                          (setf height (max 0 (- height (row-span (svref rows end)))))))))
    (when (and (< height max-height) (< end (length rows)))
      (incf end))
    (values start end)))

;;; Drawing

(defmethod render ((table table) area buffer)
  (let ((state (make-table-state)))
    ;; DRAW-TABLE doesn't keep the state, so it can live on the stack.
    (declare (dynamic-extent state))
    (draw-table table area buffer state)))

(defmethod render-stateful ((table table) area buffer (state table-state))
  (draw-table table area buffer state))

(defun draw-table (table area buffer state)
  (multiple-value-bind (x y width height) (clip-area area buffer)
    (set-style-region buffer x y width height (table-style table))
    (render (table-block table) area buffer)
    (when (table-block table)
      (setf (values x y width height) (block-inner-bounds (table-block table) x y width height)))
    (when (and (plusp width) (plusp height))
      (let ((count (length (table-rows table)))
            (selected (table-state-selected state)))
        (when (and selected (>= selected count))
          (table-select state (max 0 (1- count))))
        (when (zerop count)
          (table-select state nil)))
      (let* ((selection-width (if (selection-column-p (table-highlight-spacing table)
                                                      (table-state-selected state))
                                  (table-symbol-width table)
                                  0))
             (columns (column-areas table width selection-width)))
        (draw-sections table buffer x y width height columns selection-width state)))))

(defun draw-sections (table buffer x y width height columns selection-width state)
  "Divide the WIDTH × HEIGHT region at (X, Y) between the header, the rows and the footer and
draw them. The header takes its rows first, then the footer, and the rows get the rest."
  (declare (type fixnum x y width height))
  (let ((header (table-header table))
        (footer (table-footer table))
        (free height))
    (declare (type fixnum free))
    (flet ((take (rows)
             (declare (type fixnum rows))
             (let ((taken (min free rows)))
               (decf free taken)
               taken)))
      (let* ((header-top (if header (take (table-row-top-margin header)) 0))
             (header-height (if header (take (table-row-height header)) 0))
             (rows-y (+ y header-top header-height
                        (if header (take (table-row-bottom-margin header)) 0))))
        (when footer
          (take (table-row-bottom-margin footer)))
        (let* ((footer-height (if footer (take (table-row-height footer)) 0))
               (footer-top (if footer (take (table-row-top-margin footer)) 0)))
          (draw-fixed-row header buffer x (+ y header-top) width header-height columns)
          (draw-rows table buffer x rows-y width free columns selection-width state)
          (draw-fixed-row footer buffer x (+ rows-y free footer-top) width footer-height
                          columns))))))

(defun draw-cells (row buffer x y height columns)
  "Draw ROW's cells in their COLUMNS, HEIGHT rows tall from row Y, X being the table's left
edge."
  (declare (type fixnum x y height) (type simple-vector columns))
  (loop for cell across (table-row-cells row)
        for column across columns
        do (set-text-region buffer (+ x (rect-x column)) y (rect-width column) height cell)))

(defun draw-fixed-row (row buffer x y width height columns)
  "Draw the header or footer ROW (or nothing for NIL) in the WIDTH × HEIGHT region at (X, Y)."
  (when (and row (plusp height))
    (set-style-region buffer x y width height (table-row-style row))
    (draw-cells row buffer x y height columns)))

(defun draw-rows (table buffer x y width height columns selection-width state)
  "Draw the visible rows in the WIDTH × HEIGHT region at (X, Y), scrolled to show the
selected row, then highlight it."
  (declare (type fixnum x y width height selection-width))
  (let ((rows (table-rows table))
        (selected (table-state-selected state))
        (bottom (+ y height))
        (selected-y 0)
        (selected-height -1))
    (declare (type simple-vector rows) (type fixnum bottom selected-y selected-height))
    (when (plusp (length rows))
      (multiple-value-bind (start end)
          (visible-rows rows selected (table-state-offset state) height
                        (table-scroll-padding table))
        (setf (table-state-offset state) start)
        (loop with offset of-type fixnum = 0
              for index of-type fixnum from start below end
              for row = (svref rows index)
              for row-y of-type fixnum = (+ y offset (table-row-top-margin row))
              for row-height of-type fixnum = (max 0 (- (min (+ row-y (table-row-height row))
                                                              bottom)
                                                         row-y))
              do (set-style-region buffer x row-y width row-height (table-row-style row))
                 (when (eql index selected)
                   (when (plusp selection-width)
                     (set-text-region buffer x row-y (min selection-width width) row-height
                                (table-highlight-symbol table)))
                   (setf selected-y row-y
                         selected-height row-height))
                 (draw-cells row buffer x row-y row-height columns)
                 (incf offset (row-span row))))
      (when (>= selected-height 0)
        (set-style-region buffer x selected-y width selected-height
                      (table-row-highlight-style table))))))
