;;;; truncate.lisp — the part of a line that fits in a number of columns.
;;;;
;;;; LINE-TRUNCATION and LINE-SKIP measure in place and report positions (see
;;;; tui/text/range), so widgets can clip lines every frame without allocating;
;;;; TRUNCATE-LINE builds a new line on top of them.

(defpackage #:tui/text/truncate
  (:use #:cl)
  (:import-from #:tui/width #:do-graphemes)
  (:import-from #:tui/text #:line-spans #:span-content)
  (:import-from #:tui/text/range #:copy-line-range)
  (:export #:line-truncation
           #:line-skip
           #:truncate-line))

(in-package #:tui/text/truncate)

(declaim (ftype (function (t fixnum &key (:start-span fixnum) (:start-char fixnum))
                          (values fixnum fixnum fixnum &optional))
                line-truncation))
(defun line-truncation (line width &key (start-span 0) (start-char 0))
  "Measure the longest part of LINE that starts at position (START-SPAN, START-CHAR) and
fits in WIDTH columns. Return three values: the span index and character index of the
position where it ends (exclusive), and its width in columns. Grapheme clusters are never
split, so a wide cluster that would cross column WIDTH ends the part. Zero-width clusters
right after the last cluster that fits are included. When nothing fits, the end is the start
position. Doesn't allocate."
  (let ((spans (line-spans line))
        (used 0)
        (end-span start-span)
        (end-char start-char))
    (declare (type fixnum used end-span end-char))
    (loop for index of-type fixnum from start-span below (length spans)
          for content = (span-content (svref spans index))
          do (do-graphemes ((start end columns) content
                            :start (if (= index start-span) start-char 0))
               (when (> (+ used columns) width)
                 (return-from line-truncation (values end-span end-char used)))
               (incf used columns)
               (setf end-span index
                     end-char end)))
    (values end-span end-char used)))

(declaim (ftype (function (t fixnum &key (:start-span fixnum) (:start-char fixnum))
                          (values fixnum fixnum fixnum &optional))
                line-skip))
(defun line-skip (line columns &key (start-span 0) (start-char 0))
  "Find where LINE continues after skipping COLUMNS columns from position (START-SPAN,
START-CHAR): the first grapheme cluster that starts at least COLUMNS columns in. Return three
values: its span index and character index, and the column it starts at, counted from the
start position. That column is COLUMNS, or one more when a wide cluster straddles column
COLUMNS. When the rest of the line is narrower than COLUMNS, return the end of the line and
the rest's width. Doesn't allocate."
  (let ((spans (line-spans line))
        (skipped 0)
        (end-span start-span)
        (end-char start-char))
    (declare (type fixnum skipped end-span end-char))
    (loop for index of-type fixnum from start-span below (length spans)
          for content = (span-content (svref spans index))
          do (do-graphemes ((start end width) content
                            :start (if (= index start-span) start-char 0))
               (when (>= skipped columns)
                 (return-from line-skip (values index start skipped)))
               (incf skipped width)
               (setf end-span index
                     end-char end)))
    (values end-span end-char skipped)))

(defun truncate-line (line width)
  "Return a fresh line with the longest prefix of LINE that fits in WIDTH columns, as
LINE-TRUNCATION measures it, keeping the spans' styles and LINE's style and alignment."
  (multiple-value-bind (end-span end-char) (line-truncation line width)
    (copy-line-range line 0 0 end-span end-char)))
