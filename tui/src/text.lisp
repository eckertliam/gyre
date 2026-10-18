;;;; text.lisp — styled text: spans, lines and texts, their widths and their alignment.
;;;;
;;;; Like ratatui's Span, Line and Text these are immutable values. A span is a string
;;;; drawn in one style. A line is a vector of spans with a style and an alignment of its
;;;; own, applied under the spans'. A text is a vector of lines, likewise. Positions inside
;;;; a line, truncation and wrapping live in tui/text/range, tui/text/truncate and
;;;; tui/text/wrap.

(defpackage #:tui/text
  (:use #:cl)
  (:import-from #:tui/style #:style #:patch-style)
  (:import-from #:tui/width #:string-width)
  (:export #:alignment
           #:invalid-alignment
           #:invalid-alignment-object
           #:invalid-text-designator
           #:invalid-text-designator-object
           #:invalid-text-designator-kind
           #:span
           #:spanp
           #:span-content
           #:span-style
           #:line
           #:linep
           #:make-line
           #:line-spans
           #:line-style
           #:line-alignment
           #:text
           #:textp
           #:make-text
           #:text-from-string
           #:text-lines
           #:text-style
           #:text-alignment
           #:span-width
           #:line-width
           #:text-width
           #:text-height
           #:alignment-offset
           #:effective-alignment))

(in-package #:tui/text)

;;; Conditions

(deftype alignment ()
  "How a line sits in a wider area: :left, :center, :right, or NIL for \"unset\", which
defers to the enclosing text or widget and means :left when nothing sets one."
  '(member nil :left :center :right))

(define-condition invalid-alignment (error)
  ((object :initarg :object :reader invalid-alignment-object))
  (:report (lambda (condition stream)
             (format stream "~S is not an alignment (NIL, :left, :center or :right)."
                     (invalid-alignment-object condition))))
  (:documentation "Signaled when a value that should be an alignment is not."))

(define-condition invalid-text-designator (error)
  ((object :initarg :object :reader invalid-text-designator-object)
   (kind :initarg :kind :reader invalid-text-designator-kind))
  (:report (lambda (condition stream)
             (format stream "~S does not designate ~A."
                     (invalid-text-designator-object condition)
                     (ecase (invalid-text-designator-kind condition)
                       (:span "span content (a string)")
                       (:line "the spans of a line")
                       (:text "the lines of a text")))))
  (:documentation "Signaled when a value given to build a span, line or text doesn't
designate one. KIND is :span, :line or :text."))

(defun alignment-designator (alignment)
  (if (typep alignment 'alignment)
      alignment
      (error 'invalid-alignment :object alignment)))

;;; Spans, lines and texts

(defstruct (span (:constructor %make-span (content style))
                 (:predicate spanp)
                 (:copier nil))
  "A string drawn in one style. STYLE (or NIL) is patched over the styles of the line and
text that contain the span."
  (content "" :type simple-string :read-only t)
  (style nil :type (or null style) :read-only t))

(defstruct (line (:constructor %make-line (spans style alignment))
                 (:predicate linep)
                 (:copier nil))
  "One row of text: SPANS, a simple vector of spans drawn one after another. STYLE (or NIL)
is applied under the spans' styles, and ALIGNMENT overrides the enclosing text's."
  (spans #() :type simple-vector :read-only t)
  (style nil :type (or null style) :read-only t)
  (alignment nil :type alignment :read-only t))

(defstruct (text (:constructor %make-text (lines style alignment))
                 (:predicate textp)
                 (:copier nil))
  "Several lines: LINES, a simple vector of lines drawn one below another. STYLE (or NIL)
is applied under the lines' styles, and ALIGNMENT applies to lines that have none."
  (lines #() :type simple-vector :read-only t)
  (style nil :type (or null style) :read-only t)
  (alignment nil :type alignment :read-only t))

(defun simple-content (string)
  (if (simple-string-p string) string (coerce string 'simple-string)))

(defun span (content &optional style)
  "Return a span showing the string CONTENT in STYLE, a style or NIL. A simple string is
used as is, so it must not be modified afterwards; any other string is copied. Signal
INVALID-TEXT-DESIGNATOR when CONTENT is not a string."
  (unless (stringp content)
    (error 'invalid-text-designator :object content :kind :span))
  (check-type style (or null style))
  (%make-span (simple-content content) style))

(defun span-designator (designator)
  (typecase designator
    (span designator)
    (string (span designator))
    (t (error 'invalid-text-designator :object designator :kind :line))))

(defun spans-designator (designator)
  (typecase designator
    ((or span string) (vector (span-designator designator)))
    ((or list vector) (map 'simple-vector #'span-designator designator))
    (t (error 'invalid-text-designator :object designator :kind :line))))

(defun make-line (spans &key style alignment)
  "Return a line. SPANS is a span, a string (one unstyled span; newlines in it are not
interpreted), or a list or vector of spans and strings. STYLE is a style or NIL and
ALIGNMENT an alignment. Signal INVALID-TEXT-DESIGNATOR or INVALID-ALIGNMENT for bad
arguments."
  (check-type style (or null style))
  (%make-line (spans-designator spans) style (alignment-designator alignment)))

;;; Building texts

(defun string-line (string start end)
  (%make-line (vector (%make-span (if (and (= start 0) (= end (length string)))
                                      string
                                      (subseq string start end))
                                  nil))
              nil nil))

(defun string-lines (string)
  "The lines of STRING as a list of one-span lines. Lines end at #\\Newline, a carriage
return before it is dropped, and a final newline doesn't start another line (like Rust's
str::lines). The empty string is one empty line."
  (let ((string (simple-content string))
        (lines '())
        (start 0))
    (loop for newline = (position #\Newline string :start start)
          while newline
          do (push (string-line string start
                                (if (and (> newline start)
                                         (char= (char string (1- newline)) #\Return))
                                    (1- newline)
                                    newline))
                   lines)
             (setf start (1+ newline)))
    (when (or (< start (length string)) (null lines))
      (push (string-line string start (length string)) lines))
    (nreverse lines)))

(defun fold-line (line style alignment)
  "LINE as it is drawn inside a text with STYLE and ALIGNMENT, as a line of its own."
  (if (and (null style) (or (null alignment) (line-alignment line)))
      line
      (%make-line (line-spans line)
                  (patch-style style (line-style line))
                  (or (line-alignment line) alignment))))

(defun designated-lines (designator)
  "The lines that DESIGNATOR, an element of a text designator, contributes, as a fresh list."
  (typecase designator
    (text (map 'list (lambda (line)
                       (fold-line line (text-style designator) (text-alignment designator)))
               (text-lines designator)))
    (line (list designator))
    (span (list (%make-line (vector designator) nil nil)))
    (string (string-lines designator))
    (t (error 'invalid-text-designator :object designator :kind :text))))

(defun lines-designator (designator)
  (coerce (typecase designator
            ((or text line span string) (designated-lines designator))
            ((or list vector) (mapcan #'designated-lines (coerce designator 'list)))
            (t (error 'invalid-text-designator :object designator :kind :text)))
          'simple-vector))

(defun make-text (lines &key style alignment)
  "Return a text. LINES is a text, a line, a span, a string (split into lines as by
TEXT-FROM-STRING), or a list or vector of those. A text among LINES contributes its lines
with its own style and alignment folded into each, so they look the same. STYLE is a style
or NIL and ALIGNMENT an alignment. Signal INVALID-TEXT-DESIGNATOR or INVALID-ALIGNMENT for
bad arguments."
  (check-type style (or null style))
  (%make-text (lines-designator lines) style (alignment-designator alignment)))

(defun text-from-string (string &key style alignment)
  "Return a text with one unstyled single-span line per line of STRING. Lines end at
#\\Newline; a carriage return before it is dropped and a final newline doesn't start
another line. The empty string is one empty line. STYLE and ALIGNMENT are the text's."
  (unless (stringp string)
    (error 'invalid-text-designator :object string :kind :text))
  (check-type style (or null style))
  (%make-text (coerce (string-lines string) 'simple-vector) style
              (alignment-designator alignment)))

;;; Width

(declaim (ftype (function (span) (values fixnum &optional)) span-width))
(defun span-width (span)
  "Return how many columns SPAN occupies: the display width of its content."
  (string-width (span-content span)))

(declaim (ftype (function (line) (values fixnum &optional)) line-width))
(defun line-width (line)
  "Return how many columns LINE occupies: the sum of its spans' widths."
  (let ((width 0))
    (declare (type fixnum width))
    (loop for span across (line-spans line)
          do (incf width (span-width span)))
    width))

(declaim (ftype (function (text) (values fixnum &optional)) text-width))
(defun text-width (text)
  "Return how many columns TEXT occupies: the width of its widest line, 0 when it has none."
  (let ((width 0))
    (declare (type fixnum width))
    (loop for line across (text-lines text)
          do (setf width (max width (line-width line))))
    width))

(defun text-height (text)
  "Return how many rows TEXT occupies when not wrapped: its number of lines."
  (length (text-lines text)))

;;; Alignment

(declaim (ftype (function (alignment fixnum fixnum) (values fixnum &optional))
                alignment-offset))
(defun alignment-offset (alignment content-width area-width)
  "Return the column, relative to the area's left edge, at which content CONTENT-WIDTH
columns wide starts when aligned by ALIGNMENT in an area AREA-WIDTH columns wide: 0 for
:left or NIL, half the free columns rounded down for :center, all of them for :right. Never
negative: content wider than the area starts at 0."
  (let ((free (max 0 (- area-width content-width))))
    (ecase alignment
      ((nil :left) 0)
      (:center (floor free 2))
      (:right free))))

(defun effective-alignment (line text-alignment)
  "Return the alignment LINE is drawn with inside a text aligned by TEXT-ALIGNMENT: the
line's own alignment when it has one, otherwise TEXT-ALIGNMENT (NIL meaning :left)."
  (or (line-alignment line) text-alignment))
