;;;; scrollbar.lisp — the scrollbar widget and its state: a track with a thumb showing which
;;;; part of some content is in view, as in ratatui.

(defpackage #:tui/widgets/scrollbar
  (:use #:cl)
  (:import-from #:tui/style #:style)
  (:import-from #:tui/width #:string-width)
  (:import-from #:tui/buffer #:set-string)
  (:import-from #:tui/widgets/widget #:render-stateful)
  (:import-from #:tui/widgets/draw #:clip-area)
  (:export #:scrollbar
           #:scrollbar-state
           #:scrollbar-state-p
           #:make-scrollbar-state
           #:scrollbar-state-content-length
           #:scrollbar-state-position
           #:scrollbar-state-viewport-length))

(in-package #:tui/widgets/scrollbar)

;;; State

(defstruct (scrollbar-state (:constructor make-scrollbar-state
                                (&key (content-length 0) (position 0) (viewport-length 0)))
                            (:copier nil))
  "What a scrollbar shows: content CONTENT-LENGTH rows (or columns) long, scrolled to
POSITION, of which VIEWPORT-LENGTH are in view at once; a VIEWPORT-LENGTH of 0 means the
length of the scrollbar's area. Nothing is drawn while CONTENT-LENGTH is 0."
  (content-length 0 :type (and fixnum unsigned-byte))
  (position 0 :type (and fixnum unsigned-byte))
  (viewport-length 0 :type (and fixnum unsigned-byte)))

;;; The widget

(defun vertical-p (orientation)
  (ecase orientation
    ((:vertical-right :vertical-left) t)
    ((:horizontal-bottom :horizontal-top) nil)))

(defun symbol-string (code)
  (string (code-char code)))

(defparameter *symbol-sets*
  ;; (name orientation track thumb begin end), as ratatui's symbols::scrollbar sets.
  (mapcar (lambda (set) (list* (first set) (second set) (mapcar #'symbol-string (cddr set))))
          '((:double :vertical #x2551 #x2588 #x25B2 #x25BC)      ; ║ █ ▲ ▼
            (:double :horizontal #x2550 #x2588 #x25C4 #x25BA)    ; ═ █ ◄ ►
            (:plain :vertical #x2502 #x2588 #x2191 #x2193)       ; │ █ ↑ ↓
            (:plain :horizontal #x2500 #x2588 #x2190 #x2192)))   ; ─ █ ← →
  "The symbols of each named set for each direction: track, thumb, begin and end.")

(defun symbol-set (name orientation)
  "The symbols of the set NAME for a scrollbar with ORIENTATION: a list of the track, thumb,
begin and end symbols."
  (let ((direction (if (vertical-p orientation) :vertical :horizontal)))
    (cddr (find-if (lambda (set) (and (eq (first set) name) (eq (second set) direction)))
                   *symbol-sets*))))

(defclass scrollbar ()
  ((orientation :initarg :orientation :reader scrollbar-orientation)
   (thumb-symbol :initarg :thumb-symbol :reader scrollbar-thumb-symbol)
   (track-symbol :initarg :track-symbol :reader scrollbar-track-symbol)
   (begin-symbol :initarg :begin-symbol :reader scrollbar-begin-symbol)
   (end-symbol :initarg :end-symbol :reader scrollbar-end-symbol)
   (arrows-length :initarg :arrows-length :reader scrollbar-arrows-length)
   (thumb-style :initarg :thumb-style :reader scrollbar-thumb-style)
   (track-style :initarg :track-style :reader scrollbar-track-style)
   (begin-style :initarg :begin-style :reader scrollbar-begin-style)
   (end-style :initarg :end-style :reader scrollbar-end-style))
  (:documentation "A widget that draws a scrollbar along one edge of its area: a begin
symbol, a track with a thumb on it and an end symbol. It is rendered with a SCROLLBAR-STATE
by RENDER-STATEFUL. Make one with SCROLLBAR."))

(defun scrollbar (&key (orientation :vertical-right) (symbols :double)
                    thumb-symbol (track-symbol nil track-p)
                    (begin-symbol nil begin-p) (end-symbol nil end-p)
                    style thumb-style track-style begin-style end-style)
  "Return a scrollbar widget.

ORIENTATION is :vertical-right (the default), :vertical-left, :horizontal-bottom or
:horizontal-top: the edge of the area the scrollbar is drawn along. SYMBOLS names the set of
symbols to draw it with: :double (the default; a double line track, a full block thumb and
triangle arrows) or :plain (a single line track and arrows). THUMB-SYMBOL, TRACK-SYMBOL,
BEGIN-SYMBOL and END-SYMBOL, one-grapheme strings, replace symbols of the set; NIL for the
track leaves the track's cells as they are, and NIL for the begin or end symbol leaves it
out, giving its cell to the track.

STYLE patches every part; THUMB-STYLE, TRACK-STYLE, BEGIN-STYLE and END-STYLE patch their
part instead. Styles are styles or NIL."
  (check-type orientation (member :vertical-right :vertical-left :horizontal-bottom
                                  :horizontal-top))
  (check-type symbols (member :double :plain))
  (check-type thumb-symbol (or null string))
  (check-type track-symbol (or null string))
  (check-type begin-symbol (or null string))
  (check-type end-symbol (or null string))
  (dolist (style (list style thumb-style track-style begin-style end-style))
    (check-type style (or null style)))
  (destructuring-bind (track thumb begin end) (symbol-set symbols orientation)
    (let ((begin (if begin-p begin-symbol begin))
          (end (if end-p end-symbol end)))
      (make-instance 'scrollbar
                     :orientation orientation
                     :thumb-symbol (or thumb-symbol thumb)
                     :track-symbol (if track-p track-symbol track)
                     :begin-symbol begin
                     :end-symbol end
                     :arrows-length (+ (if begin (string-width begin) 0)
                                       (if end (string-width end) 0))
                     :thumb-style (or thumb-style style)
                     :track-style (or track-style style)
                     :begin-style (or begin-style style)
                     :end-style (or end-style style)))))

;;; Thumb size and position

(defun track-length (scrollbar width height)
  "The number of cells between the begin and end symbols of SCROLLBAR in an area of WIDTH ×
HEIGHT cells."
  (declare (type fixnum width height))
  (max 0 (- (if (vertical-p (scrollbar-orientation scrollbar)) height width)
            (scrollbar-arrows-length scrollbar))))

(defun part-lengths (scrollbar state width height)
  "Return the lengths of the track before the thumb, of the thumb and of the track after it,
for SCROLLBAR with STATE in an area of WIDTH × HEIGHT cells. This is ratatui's arithmetic:
the thumb is to the track as the viewport is to the content plus a viewport (the last
position shows the last item at the top), both rounded to the nearest cell, with the thumb at
least one cell long and kept inside the track."
  (declare (type fixnum width height))
  (flet ((rounding-divide (numerator denominator)
           (floor (+ numerator (floor denominator 2)) denominator)))
    (let ((track (track-length scrollbar width height)))
      (if (zerop track)
          (values 0 0 0)
          (let* ((viewport (let ((length (scrollbar-state-viewport-length state)))
                             (cond ((plusp length) length)
                                   ((vertical-p (scrollbar-orientation scrollbar)) height)
                                   (t width))))
                 (max-position (max 0 (1- (scrollbar-state-content-length state))))
                 (position (min (scrollbar-state-position state) max-position))
                 (max-viewport-position (+ max-position viewport)))
            (if (zerop max-viewport-position)
                (values 0 track 0)
                (let* ((thumb (max 1 (min track (rounding-divide (* viewport track)
                                                                 max-viewport-position))))
                       (start (min (- track thumb)
                                   (rounding-divide (* position track) max-viewport-position))))
                  (values start thumb (- track start thumb)))))))))

;;; Drawing

(defmethod render-stateful ((scrollbar scrollbar) area buffer (state scrollbar-state))
  (multiple-value-bind (x y width height) (clip-area area buffer)
    (when (and (plusp width) (plusp height)
               (plusp (scrollbar-state-content-length state))
               (plusp (track-length scrollbar width height)))
      (multiple-value-bind (before thumb after) (part-lengths scrollbar state width height)
        (draw-scrollbar scrollbar buffer x y width height before thumb after)))))

(defun draw-scrollbar (scrollbar buffer x y width height before thumb after)
  "Draw the begin symbol, BEFORE track cells, THUMB thumb cells, AFTER track cells and the
end symbol, one per cell along SCROLLBAR's edge of the WIDTH × HEIGHT region at (X, Y)."
  (declare (type fixnum x y width height before thumb after))
  (let* ((orientation (scrollbar-orientation scrollbar))
         (vertical (vertical-p orientation))
         ;; The column of a vertical bar, the row of a horizontal one.
         (edge (ecase orientation
                 (:vertical-left x)
                 (:vertical-right (+ x width -1))
                 (:horizontal-top y)
                 (:horizontal-bottom (+ y height -1))))
         (index 0))
    (declare (type fixnum edge index))
    (flet ((put (symbol style)
             (when symbol
               (let ((column (if vertical edge (+ x index))))
                 (set-string buffer column (if vertical (+ y index) edge) symbol
                             :style style :max-width (- (+ x width) column))))
             (incf index)))
      (when (scrollbar-begin-symbol scrollbar)
        (put (scrollbar-begin-symbol scrollbar) (scrollbar-begin-style scrollbar)))
      (dotimes (i before)
        (put (scrollbar-track-symbol scrollbar) (scrollbar-track-style scrollbar)))
      (dotimes (i thumb)
        (put (scrollbar-thumb-symbol scrollbar) (scrollbar-thumb-style scrollbar)))
      (dotimes (i after)
        (put (scrollbar-track-symbol scrollbar) (scrollbar-track-style scrollbar)))
      (when (scrollbar-end-symbol scrollbar)
        (put (scrollbar-end-symbol scrollbar) (scrollbar-end-style scrollbar))))))
