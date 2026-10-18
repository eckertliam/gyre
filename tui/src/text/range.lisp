;;;; range.lisp — ranges of characters in a line, between two positions.
;;;;
;;;; A position in a line is a span index and a character index into that span's content.
;;;; Truncation and wrapping report the parts of a line they keep as a pair of positions, so
;;;; that drawing them doesn't need a copy of the line.

(defpackage #:tui/text/range
  (:use #:cl)
  (:import-from #:tui/text
                #:span #:span-content #:span-style #:make-line
                #:line-spans #:line-style #:line-alignment)
  (:export #:map-line-range
           #:copy-line-range))

(in-package #:tui/text/range)

(declaim (ftype (function (t t fixnum fixnum fixnum fixnum) (values null &optional))
                map-line-range))
(defun map-line-range (function line start-span start-char end-span end-char)
  "Call FUNCTION with (SPAN START END) for each span of LINE that has characters from
position (START-SPAN, START-CHAR) up to, not including, position (END-SPAN, END-CHAR), in
order; START and END bound those characters in the span's content. A position is a span
index and a character index into that span's content; the end of one span is the same
place as the start of the next. Return NIL. Doesn't allocate unless FUNCTION does."
  (let ((function (coerce function 'function))
        (spans (line-spans line)))
    (loop for index of-type fixnum from start-span to (min end-span (1- (length spans)))
          for span = (svref spans index)
          for from of-type fixnum = (if (= index start-span) start-char 0)
          for to of-type fixnum = (if (= index end-span) end-char (length (span-content span)))
          when (< from to)
            do (funcall function span from to))
    nil))

(defun copy-line-range (line start-span start-char end-span end-char
                        &key (style (line-style line)) (alignment (line-alignment line)))
  "Return a fresh line holding the characters of LINE between positions (START-SPAN,
START-CHAR) and (END-SPAN, END-CHAR), as MAP-LINE-RANGE delimits them, each part keeping
its span's style. Spans that lie wholly inside the range are shared rather than copied. The
new line's STYLE and ALIGNMENT default to LINE's."
  (let ((parts '()))
    (map-line-range (lambda (part start end)
                      (let ((content (span-content part)))
                        (push (if (and (= start 0) (= end (length content)))
                                  part
                                  (span (subseq content start end) (span-style part)))
                              parts)))
                    line start-span start-char end-span end-char)
    (make-line (nreverse parts) :style style :alignment alignment)))
