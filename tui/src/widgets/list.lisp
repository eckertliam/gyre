;;;; list.lisp — the list widget: items one below another with a selection that scrolls into
;;;; view, as in ratatui. Its state is a LIST-STATE (tui/widgets/list-state).
;;;;
;;;; LIST is also a CL symbol. This package shadows it, so the class and its constructor are
;;;; TUI/WIDGETS/LIST:LIST (re-exported as TUI:LIST) and CL's function is CL:LIST.

(defpackage #:tui/widgets/list
  (:use #:cl)
  (:shadow #:list)
  (:shadowing-import-from #:tui/widgets/block #:block)
  (:import-from #:tui/widgets/block #:block-inner-bounds)
  (:import-from #:tui/style #:style)
  (:import-from #:tui/buffer #:set-string #:set-style-region)
  (:import-from #:tui/text #:text-height #:line-width #:line-alignment)
  (:import-from #:tui/widgets/widget #:render #:render-stateful)
  (:import-from #:tui/widgets/draw #:clip-area #:as-text #:as-line #:highlight-spacing
                #:selection-column-p)
  (:import-from #:tui/buffer/text #:set-aligned-line #:set-text-region)
  (:import-from #:tui/widgets/list-state
                #:list-state #:make-list-state #:list-state-selected #:list-state-offset
                #:list-select)
  (:export #:list))

(in-package #:tui/widgets/list)

(deftype fixnum-vector ()
  '(simple-array fixnum (*)))

(defclass list ()
  ((items :initarg :items :reader list-items)
   (heights :initarg :heights :reader list-heights)
   (block :initarg :block :reader list-block)
   (style :initarg :style :reader list-style)
   (highlight-style :initarg :highlight-style :reader list-highlight-style)
   (highlight-symbol :initarg :highlight-symbol :reader list-highlight-symbol)
   (symbol-width :initarg :symbol-width :reader list-symbol-width)
   (blank-symbol :initarg :blank-symbol :reader list-blank-symbol)
   (repeat-highlight-symbol :initarg :repeat-highlight-symbol
                            :reader list-repeat-highlight-symbol)
   (highlight-spacing :initarg :highlight-spacing :reader list-highlight-spacing)
   (direction :initarg :direction :reader list-direction)
   (scroll-padding :initarg :scroll-padding :reader list-scroll-padding))
  (:documentation "A widget that shows items one below another, one of which can be selected
and highlighted. Its state, a LIST-STATE, holds the selection and the scroll offset. Make one
with LIST."))

(defun list (items &key block style highlight-style highlight-symbol repeat-highlight-symbol
                     (highlight-spacing :when-selected) (direction :top-to-bottom)
                     (scroll-padding 0))
  "Return a list widget showing ITEMS, a list or vector of text designators (see MAKE-TEXT):
each item is a text and takes as many rows as it has lines.

BLOCK, a block or NIL, is drawn around the items. STYLE patches the whole area, under the
items' own styles. HIGHLIGHT-STYLE patches the rows of the selected item after it is drawn.

HIGHLIGHT-SYMBOL, a line designator (see MAKE-LINE) or NIL, is drawn left of the selected
item's first row, or of each of its rows with REPEAT-HIGHLIGHT-SYMBOL; other rows get that
many spaces. HIGHLIGHT-SPACING says when the column for the symbol is reserved: :always,
:when-selected (the default: only while an item is selected) or :never.

DIRECTION is :top-to-bottom (the default) or :bottom-to-top, which puts the first item at the
bottom. When an item is selected, rendering scrolls the list so that it is visible, with
SCROLL-PADDING more items on either side when they fit."
  (check-type block (or null block))
  (check-type style (or null style))
  (check-type highlight-style (or null style))
  (check-type highlight-spacing highlight-spacing)
  (check-type direction (member :top-to-bottom :bottom-to-top))
  (check-type scroll-padding (and fixnum unsigned-byte))
  (check-type items sequence)
  (let* ((items (map 'simple-vector #'as-text items))
         (symbol (and highlight-symbol (as-line highlight-symbol)))
         (symbol-width (if symbol (line-width symbol) 0)))
    (make-instance 'list
                   :items items
                   :heights (map 'fixnum-vector #'text-height items)
                   :block block
                   :style style
                   :highlight-style highlight-style
                   :highlight-symbol symbol
                   :symbol-width symbol-width
                   :blank-symbol (make-string symbol-width :initial-element #\Space)
                   :repeat-highlight-symbol (and repeat-highlight-symbol t)
                   :highlight-spacing highlight-spacing
                   :direction direction
                   :scroll-padding scroll-padding)))

;;; Which items are visible
;;;
;;; These are ratatui's List::get_items_bounds and apply_scroll_padding_to_selected_index.

(defun padded-selection (heights selected max-height first last padding)
  "The index of the item that has to be visible for the selected item SELECTED to be shown
with PADDING items around it, given that items FIRST to LAST (exclusive) are visible now.
The padding shrinks until the padded items fit in MAX-HEIGHT rows."
  (declare (type fixnum-vector heights) (type fixnum selected max-height first last padding))
  (let* ((last-valid (1- (length heights)))
         (selected (min selected last-valid))
         (padding (min padding last-valid)))   ; more padding than items changes nothing
    (declare (type fixnum padding))
    (loop while (plusp padding)
          until (<= (loop for i from (max 0 (- selected padding))
                            to (min (+ selected padding) last-valid)
                          sum (aref heights i))
                    max-height)
          do (decf padding))
    (min last-valid
         (cond ((>= (min (+ selected padding) last-valid) last) (+ selected padding))
               ((< (max 0 (- selected padding)) first) (max 0 (- selected padding)))
               (t selected)))))

(defun visible-items (heights selected offset max-height padding)
  "Return the indices of the first visible item and of the item after the last one, for
items of HEIGHTS shown in MAX-HEIGHT rows from item OFFSET, scrolled to show the selected
item SELECTED (or NIL) with PADDING items around it."
  (declare (type fixnum-vector heights) (type fixnum offset max-height padding))
  (let* ((offset (min offset (max 0 (1- (length heights)))))
         (first offset)
         (last offset)
         (height 0))
    (declare (type fixnum first last height))
    (loop for i from offset below (length heights)
          while (<= (+ height (aref heights i)) max-height)
          do (incf height (aref heights i))
             (incf last))
    (let ((target (if selected
                      (padded-selection heights selected max-height first last padding)
                      offset)))
      ;; Scroll down until the target is visible, dropping items from the top.
      (loop while (>= target last)
            do (incf height (aref heights last))
               (incf last)
               (loop while (> height max-height)
                     do (decf height (aref heights first))
                        (incf first)))
      ;; Scroll up until it is visible, dropping items from the bottom.
      (loop while (< target first)
            do (decf first)
               (incf height (aref heights first))
               (loop while (> height max-height)
                     do (decf last)
                        (decf height (aref heights last)))))
    (values first last)))

;;; Drawing

(defmethod render ((list list) area buffer)
  (let ((state (make-list-state)))
    ;; DRAW-LIST doesn't keep the state, so it can live on the stack.
    (declare (dynamic-extent state))
    (draw-list list area buffer state)))

(defmethod render-stateful ((list list) area buffer (state list-state))
  (draw-list list area buffer state))

(defun draw-list (list area buffer state)
  (multiple-value-bind (x y width height) (clip-area area buffer)
    (set-style-region buffer x y width height (list-style list))
    (render (list-block list) area buffer)
    (when (list-block list)
      (setf (values x y width height) (block-inner-bounds (list-block list) x y width height)))
    (when (and (plusp width) (plusp height))
      (let ((count (length (list-items list)))
            (selected (list-state-selected state)))
        (cond ((zerop count)
               (list-select state nil))
              (t
               (when (and selected (>= selected count))
                 (list-select state (1- count)))
               (multiple-value-bind (first last)
                   (visible-items (list-heights list) (list-state-selected state)
                                  (list-state-offset state) height
                                  (list-scroll-padding list))
                 (setf (list-state-offset state) first)
                 (draw-items list buffer x y width height first last
                             (list-state-selected state)))))))))

(defun draw-items (list buffer x y width height first last selected)
  "Draw items FIRST to LAST (exclusive) of LIST in the WIDTH × HEIGHT region at (X, Y)."
  (declare (type fixnum x y width height first last))
  (let* ((heights (list-heights list))
         (bottom-to-top (eq (list-direction list) :bottom-to-top))
         (symbol-width (if (selection-column-p (list-highlight-spacing list) selected)
                           (min (list-symbol-width list) width)
                           0))
         (used 0))
    (declare (type fixnum-vector heights) (type fixnum symbol-width used))
    (loop for index of-type fixnum from first below last
          for item-height = (aref heights index)
          for item-y of-type fixnum = (if bottom-to-top
                                          (- (+ y height) used item-height)
                                          (+ y used))
          ;; Only rows inside the region are drawn, even for an item taller than it.
          for top of-type fixnum = (max item-y y)
          for rows of-type fixnum = (- (min (+ item-y item-height) (+ y height)) top)
          do (incf used item-height)
             (when (plusp rows)
               (set-text-region buffer (+ x symbol-width) top (- width symbol-width) rows
                          (svref (list-items list) index) (- top item-y))
               (when (eql index selected)
                 (set-style-region buffer x top width rows (list-highlight-style list)))
               (when (plusp symbol-width)
                 (draw-symbols list buffer x top symbol-width rows (- top item-y)
                               (eql index selected)))))))

(defun draw-symbols (list buffer x y symbol-width rows first-row selectedp)
  "Draw the highlight symbol column, SYMBOL-WIDTH columns wide, for ROWS rows of an item from
(X, Y), the first of them being row FIRST-ROW of the item."
  (declare (type fixnum x y symbol-width rows first-row))
  (loop for row of-type fixnum from 0 below rows
        do (if (and selectedp
                    (or (= (+ first-row row) 0) (list-repeat-highlight-symbol list)))
               (let ((symbol (list-highlight-symbol list)))
                 (set-aligned-line buffer x (+ y row) symbol-width symbol (line-alignment symbol)))
               (set-string buffer x (+ y row) (list-blank-symbol list)
                           :max-width symbol-width))))
