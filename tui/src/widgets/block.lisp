;;;; block.lisp — the block widget: borders, titles and padding around an area, as in ratatui.
;;;;
;;;; BLOCK is also a CL symbol. This package shadows it, so the class and its constructor are
;;;; TUI/WIDGETS/BLOCK:BLOCK (re-exported as TUI:BLOCK) and CL's special operator is CL:BLOCK.

(defpackage #:tui/widgets/block
  (:use #:cl)
  (:shadow #:block)
  (:import-from #:tui/layout #:make-rect #:rect-x #:rect-y #:rect-width #:rect-height)
  (:import-from #:tui/style #:style)
  (:import-from #:tui/buffer #:set-cell #:set-style-region)
  (:import-from #:tui/text #:line #:line-alignment #:line-width)
  (:import-from #:tui/widgets/widget #:render)
  (:import-from #:tui/widgets/draw #:clip-area #:as-line)
  (:import-from #:tui/buffer/text #:set-aligned-line)
  (:import-from #:tui/widgets/border #:border-type #:find-border-set #:border-set-top-left
                #:border-set-top-right #:border-set-bottom-left #:border-set-bottom-right
                #:border-set-vertical-left #:border-set-vertical-right #:border-set-horizontal-top
                #:border-set-horizontal-bottom)
  (:export #:block
           #:block-inner
           #:block-inner-bounds))

(in-package #:tui/widgets/block)

;;; Designators

(defconstant +top+ 1)
(defconstant +right+ 2)
(defconstant +bottom+ 4)
(defconstant +left+ 8)

(defun border-sides-p (object)
  (and (listp object)
       (every (lambda (side) (member side '(:top :right :bottom :left))) object)))

(deftype borders-designator ()
  "Which sides of a block have a border: :all, :none, or a list of :top, :right, :bottom and
:left."
  '(or (member :all :none) (satisfies border-sides-p)))

(defun side-bit (side)
  (ecase side (:top +top+) (:right +right+) (:bottom +bottom+) (:left +left+)))

(defun border-bits (borders)
  "The sides BORDERS designates, as a mask of +TOP+, +RIGHT+, +BOTTOM+ and +LEFT+."
  (check-type borders borders-designator)
  (etypecase borders
    ((eql :all) (logior +top+ +right+ +bottom+ +left+))
    ((eql :none) 0)
    (cl:list (loop with bits = 0
                   for side in borders
                   do (setf bits (logior bits (side-bit side)))
                   finally (return bits)))))

(defun padding-list-p (object)
  (and (listp object)
       (= (length object) 4)
       (every (lambda (n) (typep n '(and fixnum unsigned-byte))) object)))

(deftype padding-designator ()
  "Empty columns and rows inside a block's borders: a non-negative fixnum for every side, or
a list of four, (left right top bottom)."
  '(or (and fixnum unsigned-byte) (satisfies padding-list-p)))

;;; Titles

(defstruct (title (:constructor make-title (line position alignment width))
                  (:copier nil)
                  (:predicate nil))
  "A title of a block: LINE, drawn WIDTH columns wide at the top or bottom (POSITION),
grouped with the titles of the same ALIGNMENT."
  (line nil :type line :read-only t)
  (position :top :type (member :top :bottom) :read-only t)
  (alignment :left :type (member :left :center :right) :read-only t)
  (width 0 :type fixnum :read-only t))

(defun parse-title (designator position alignment)
  "Return the title DESIGNATOR describes: a line designator (see MAKE-LINE), shown at
POSITION, or a list (:top line-designator) or (:bottom line-designator). A line without an
alignment of its own goes with ALIGNMENT."
  (multiple-value-bind (position content)
      (if (and (consp designator) (member (first designator) '(:top :bottom)))
          (values (first designator) (second designator))
          (values position designator))
    (let ((line (as-line content)))
      (make-title line position (or (line-alignment line) alignment) (line-width line)))))

;;; The widget

(defclass block ()
  ((borders :initarg :borders :reader block-borders)
   (border-set :initarg :border-set :reader block-border-set)
   (border-style :initarg :border-style :reader block-border-style)
   (style :initarg :style :reader block-style)
   (title-style :initarg :title-style :reader block-title-style)
   (titles :initarg :titles :reader block-titles)
   (top-title-p :initarg :top-title-p :reader block-top-title-p)
   (bottom-title-p :initarg :bottom-title-p :reader block-bottom-title-p)
   (padding :initarg :padding :reader block-padding))
  (:documentation "A widget that draws borders and titles around an area and patches it with
a style. Other widgets take a block to draw around their content. Make one with BLOCK."))

(defun block (&key (borders :none) (border-type :plain) border-style style title titles
                (title-alignment :left) (title-position :top) title-style (padding 0))
  "Return a block widget.

BORDERS is :all, :none (the default) or a list of the sides that have a border, :top,
:right, :bottom and :left. BORDER-TYPE is :plain (the default), :rounded, :double or :thick.
BORDER-STYLE patches the border cells.

TITLE is a title and TITLES a list of more; TITLE comes first. A title is a line designator
(a string, span or line; see MAKE-LINE), shown at TITLE-POSITION (:top, the default, or
:bottom), or a list (:top line-designator) or (:bottom line-designator). A title is aligned
by its line's alignment, or else by TITLE-ALIGNMENT (:left, the default, :center or :right).
Titles with the same position and alignment are drawn in order, one column apart; left
titles are drawn first, then centered ones, then right ones, each over the previous. Titles
are drawn over the border, between the corners, keeping its style patched with TITLE-STYLE
and their own styles. A title at a side without a border still takes a row from the inner
area.

STYLE patches the whole area. PADDING is the number of empty columns and rows inside the
borders: a non-negative fixnum for every side, or a list of four, (left right top bottom).
BORDER-STYLE, STYLE and TITLE-STYLE are styles or NIL."
  (check-type border-type border-type)
  (check-type border-style (or null style))
  (check-type style (or null style))
  (check-type title-style (or null style))
  (check-type title-alignment (member :left :center :right))
  (check-type title-position (member :top :bottom))
  (check-type titles cl:list)
  (check-type padding padding-designator)
  (let ((titles (map 'simple-vector
                     (lambda (designator) (parse-title designator title-position title-alignment))
                     (if title (cons title titles) titles))))
    (flet ((has-title-at (position) (find position titles :key #'title-position)))
      (make-instance 'block
                     :borders (border-bits borders)
                     :border-set (find-border-set border-type)
                     :border-style border-style
                     :style style
                     :title-style title-style
                     :titles titles
                     :top-title-p (and (has-title-at :top) t)
                     :bottom-title-p (and (has-title-at :bottom) t)
                     :padding (if (listp padding)
                                  (coerce padding 'simple-vector)
                                  (make-array 4 :initial-element padding))))))

;;; The inner area

(defun block-inner-bounds (block x y width height)
  "Return the area inside BLOCK's borders, title rows and padding when BLOCK is drawn in the
WIDTH × HEIGHT region at (X, Y), as four values X, Y, WIDTH and HEIGHT. The width or height
is 0 when the borders and padding don't leave room. Doesn't allocate."
  (declare (type fixnum x y width height))
  (let ((borders (block-borders block))
        (padding (block-padding block)))
    (declare (type fixnum borders) (type simple-vector padding))
    (when (logtest borders +left+)
      (setf x (min (1+ x) (+ x width))
            width (max 0 (1- width))))
    (when (or (logtest borders +top+) (block-top-title-p block))
      (setf y (min (1+ y) (+ y height))
            height (max 0 (1- height))))
    (when (logtest borders +right+)
      (setf width (max 0 (1- width))))
    (when (or (logtest borders +bottom+) (block-bottom-title-p block))
      (setf height (max 0 (1- height))))
    (let ((left (svref padding 0)) (right (svref padding 1))
          (top (svref padding 2)) (bottom (svref padding 3)))
      (declare (type fixnum left right top bottom))
      (values (+ x left) (+ y top)
              (max 0 (- width left right)) (max 0 (- height top bottom))))))

(defun block-inner (block area)
  "Return the rect inside BLOCK's borders, title rows and padding when BLOCK is drawn in the
rect AREA. See BLOCK-INNER-BOUNDS for a version that doesn't allocate."
  (multiple-value-call #'make-rect
    (block-inner-bounds block (rect-x area) (rect-y area) (rect-width area) (rect-height area))))

;;; Drawing

(defmethod render ((block block) area buffer)
  (multiple-value-bind (x y width height) (clip-area area buffer)
    (when (and (plusp width) (plusp height))
      (set-style-region buffer x y width height (block-style block))
      (draw-borders block buffer x y width height)
      (draw-titles block buffer x y width height :top)
      (draw-titles block buffer x y width height :bottom))))

(defun draw-borders (block buffer x y width height)
  "Draw BLOCK's borders around the WIDTH × HEIGHT region at (X, Y): the sides, then the
corners where two sides meet."
  (declare (type fixnum x y width height))
  (let ((borders (block-borders block))
        (set (block-border-set block))
        (style (block-border-style block))
        (right (+ x width -1))
        (bottom (+ y height -1)))
    (declare (type fixnum borders right bottom))
    (flet ((put (column row symbol)
             (set-cell buffer column row symbol style))
           (sides-p (bits)
             (= bits (logand borders bits))))
      (when (sides-p +left+)
        (loop for row from y to bottom do (put x row (border-set-vertical-left set))))
      (when (sides-p +top+)
        (loop for column from x to right do (put column y (border-set-horizontal-top set))))
      (when (sides-p +right+)
        (loop for row from y to bottom do (put right row (border-set-vertical-right set))))
      (when (sides-p +bottom+)
        (loop for column from x to right
              do (put column bottom (border-set-horizontal-bottom set))))
      (when (sides-p (logior +right+ +bottom+)) (put right bottom (border-set-bottom-right set)))
      (when (sides-p (logior +right+ +top+)) (put right y (border-set-top-right set)))
      (when (sides-p (logior +left+ +bottom+)) (put x bottom (border-set-bottom-left set)))
      (when (sides-p (logior +left+ +top+)) (put x y (border-set-top-left set))))))

(defun draw-title (block buffer x y width title alignment)
  "Draw TITLE in the WIDTH columns at (X, Y), truncated as ALIGNMENT says (see SET-ALIGNED-LINE)."
  (set-style-region buffer x y width 1 (block-title-style block))
  (set-aligned-line buffer x y width (title-line title) alignment))

(defun draw-titles (block buffer x y width height position)
  "Draw BLOCK's titles at POSITION, :top or :bottom, of the WIDTH × HEIGHT region at (X, Y):
on the border row, between the left and right borders."
  (declare (type fixnum x y width height))
  (let* ((borders (block-borders block))
         (left (if (logtest borders +left+) 1 0))
         (right (if (logtest borders +right+) 1 0)))
    (declare (type fixnum borders))
    (let ((x (+ x left))
          (y (if (eq position :top) y (+ y height -1)))
          (width (max 0 (- width left right))))
      (draw-left-titles block buffer x y width position)
      (draw-center-titles block buffer x y width position)
      (draw-right-titles block buffer x y width position))))

(defmacro do-titles ((title block position alignment &key from-end) &body body)
  "Evaluate BODY with TITLE bound to each of BLOCK's titles at POSITION with ALIGNMENT, in
order or, with FROM-END, in reverse order."
  (let ((titles (gensym "TITLES")) (index (gensym "INDEX")))
    `(let ((,titles (block-titles ,block)))
       (declare (type simple-vector ,titles))
       (loop for ,index of-type fixnum
               ,@(if from-end
                     `(from (1- (length ,titles)) downto 0)
                     `(from 0 below (length ,titles)))
             for ,title = (svref ,titles ,index)
             when (and (eq (title-position ,title) ,position)
                       (eq (title-alignment ,title) ,alignment))
               do (progn ,@body)))))

(defun draw-left-titles (block buffer x y width position)
  (declare (type fixnum x y width))
  (do-titles (title block position :left)
    (unless (plusp width)
      (return))
    (let ((title-width (title-width title)))
      (draw-title block buffer x y (min title-width width) title
                  (line-alignment (title-line title)))
      (incf x (1+ title-width))
      (setf width (max 0 (- width title-width 1))))))

(defun draw-right-titles (block buffer x y width position)
  (declare (type fixnum x y width))
  (do-titles (title block position :right :from-end t)
    (unless (plusp width)
      (return))
    (let ((title-width (title-width title)))
      (draw-title block buffer (max x (- (+ x width) title-width)) y (min title-width width)
                  title (line-alignment (title-line title)))
      (setf width (max 0 (- width title-width 1))))))

(defun draw-center-titles (block buffer x y width position)
  "Draw the centered titles one column apart, as a group centered in the WIDTH columns at
(X, Y). When the group is wider, cut its left and right ends evenly."
  (declare (type fixnum x y width))
  (let ((total -1))
    (declare (type fixnum total))
    (do-titles (title block position :center)
      (incf total (1+ (title-width title))))
    (if (<= total width)
        (let ((x (+ x (floor (- width (max 0 total)) 2))))
          (declare (type fixnum x))
          (do-titles (title block position :center)
            (draw-title block buffer x y (title-width title) title
                        (line-alignment (title-line title)))
            (incf x (1+ (title-width title)))))
        (let ((offset (floor (- total width) 2)))
          (declare (type fixnum offset))
          (do-titles (title block position :center)
            (unless (plusp width)
              (return))
            (let ((shown (max 0 (- (min width (title-width title)) offset))))
              (cond ((plusp offset)
                     (draw-title block buffer x y shown title :right)
                     (setf offset (max 0 (- offset shown 1))))
                    (t
                     (draw-title block buffer x y shown title :left)))
              (incf x (1+ shown))
              (setf width (max 0 (- width shown 1)))))))))
