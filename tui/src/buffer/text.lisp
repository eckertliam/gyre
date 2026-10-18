;;;; text.lisp — writing styled text (spans, lines and texts) into a buffer.

(defpackage #:tui/buffer/text
  (:use #:cl)
  (:import-from #:tui/layout #:rect #:make-rect #:rect-x #:rect-y #:rect-width #:rect-height)
  (:import-from #:tui/buffer #:set-string #:set-style)
  (:import-from #:tui/text
                #:span-content #:span-style #:line-spans #:line-style #:line-width
                #:text-lines #:text-style #:text-alignment #:alignment-offset
                #:effective-alignment)
  (:import-from #:tui/text/truncate #:line-skip)
  (:export #:set-span
           #:set-line
           #:set-text))

(in-package #:tui/buffer/text)

(defun set-span (buffer x y span &key max-width)
  "Write SPAN into row Y of BUFFER from column X in the span's style, as SET-STRING does.
Return the number of columns used."
  (set-string buffer x y (span-content span) :style (span-style span) :max-width max-width))

(defun write-spans (buffer x y line max-width start-span start-char)
  "Write LINE's spans from position (START-SPAN, START-CHAR) at column X of row Y, in at
most MAX-WIDTH columns (NIL for no limit), each in the line's style patched with its own.
Return the number of columns used."
  (let ((spans (line-spans line))
        (used 0))
    (declare (type fixnum used))
    (loop for index from start-span below (length spans)
          for span = (svref spans index)
          for remaining = (and max-width (- max-width used))
          while (or (null remaining) (plusp remaining))
          do (incf used (set-string buffer (+ x used) y (span-content span)
                                    :base-style (line-style line) :style (span-style span)
                                    :max-width remaining
                                    :start (if (= index start-span) start-char 0))))
    used))

(defun set-line (buffer x y line &key max-width)
  "Write LINE into row Y of BUFFER from column X, each span in the line's style patched
with the span's, stopping at the buffer's edge or after MAX-WIDTH columns. The line's
alignment is ignored; see SET-TEXT. Return the number of columns used."
  (write-spans buffer x y line max-width 0 0))

(defun set-aligned-line (buffer area row line alignment)
  "Write LINE into row ROW of AREA, aligned by ALIGNMENT. A line wider than AREA keeps
its left part (:left), its middle (:center) or its right part (:right)."
  (let* ((width (rect-width area))
         (line-width (line-width line))
         (y (+ (rect-y area) row)))
    (set-style buffer (make-rect (rect-x area) y width 1) (line-style line))
    (if (<= line-width width)
        (write-spans buffer (+ (rect-x area) (alignment-offset alignment line-width width)) y
                     line width 0 0)
        (let ((skip (ecase alignment
                      ((nil :left) 0)
                      (:center (floor (- line-width width) 2))
                      (:right (- line-width width)))))
          (multiple-value-bind (span char column) (line-skip line skip)
            (let ((indent (- column skip)))
              (write-spans buffer (+ (rect-x area) indent) y line (- width indent)
                           span char)))))))

(defun set-text (buffer area text)
  "Draw TEXT into the rect AREA of BUFFER like ratatui's Text widget: patch the area with
the text's style, then write one line per row from the top, each aligned by its own
alignment or else the text's, and clipped to the area."
  (set-style buffer area (text-style text))
  (loop for line across (text-lines text)
        for row from 0 below (rect-height area)
        do (set-aligned-line buffer area row line
                             (effective-alignment line (text-alignment text)))))
