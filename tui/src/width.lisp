;;;; width.lisp — grapheme clusters and their display width in terminal columns.
;;;;
;;;; Segmentation follows the extended grapheme cluster rules of UAX #29 exactly as
;;;; SB-UNICODE:GRAPHEMES implements them, but walks the string in place so that measuring
;;;; and drawing text never allocates.

(defpackage #:tui/width
  (:use #:cl)
  (:import-from #:tui/unicode #:char-width #:break-class #:extended-pictographic-p #:emoji-p)
  (:export #:char-width
           #:grapheme-end
           #:grapheme-width
           #:uncertain-width-p
           #:string-width
           #:do-graphemes))

(in-package #:tui/width)

;;; Character width

(declaim (inline control-code-p))
(defun control-code-p (code)
  (declare (type fixnum code))
  (or (< code #x20) (<= #x7F code #x9F)))

;;; Grapheme cluster boundaries

(defun hangul-continues-p (c1 c2)
  ;; GB6–GB8: Hangul syllable sequences stay together.
  (or (and (eq c1 :l) (member c2 '(:l :v :lv :lvt)))
      (and (member c1 '(:lv :v)) (member c2 '(:v :t)))
      (and (member c1 '(:lvt :t)) (eq c2 :t))))

(defun boundaryp (c1 c2 char2 pictographic-state ri-count)
  "True when there is a cluster boundary between a character of break class C1 and CHAR2
of break class C2. PICTOGRAPHIC-STATE describes the text before CHAR2 for GB11 and
RI-COUNT counts the regional indicators ending at CHAR2 for GB12–13."
  (cond ((and (eq c1 :cr) (eq c2 :lf)) nil)
        ((or (member c1 '(:control :cr :lf)) (member c2 '(:control :cr :lf))) t)
        ((hangul-continues-p c1 c2) nil)
        ((member c2 '(:extend :zwj :spacing-mark)) nil)
        ((eq c1 :prepend) nil)
        ((and (eq pictographic-state :zwj) (extended-pictographic-p char2)) nil)
        ((and (eq c1 :regional-indicator) (eq c2 :regional-indicator) (evenp ri-count)) nil)
        (t t)))

(defun next-pictographic-state (state char1 c1)
  ;; Tracks "Extended_Pictographic Extend* ZWJ" ending at CHAR1, for GB11.
  (cond ((extended-pictographic-p char1) :start)
        ((and (member state '(:start :extend)) (eq c1 :extend)) :extend)
        ((and (member state '(:start :extend)) (eq c1 :zwj)) :zwj)
        (t nil)))

(defun slow-grapheme-end (string start end)
  (declare (type simple-string string) (type fixnum start end))
  (let* ((char1 (schar string start))
         (c1 (break-class char1))
         (state nil)
         (ri-count (if (eq c1 :regional-indicator) 1 0)))
    (declare (type fixnum ri-count))
    (loop for index of-type fixnum from (1+ start) below end
          for char2 = (schar string index)
          for c2 = (break-class char2)
          do (setf ri-count (if (eq c2 :regional-indicator) (1+ ri-count) 0)
                   state (next-pictographic-state state char1 c1))
             (when (boundaryp c1 c2 char2 state ri-count)
               (return-from slow-grapheme-end index))
             (setf char1 char2 c1 c2))
    end))

(declaim (ftype (function (simple-string fixnum fixnum) (values fixnum &optional)) grapheme-end))
(defun grapheme-end (string start end)
  "Return the index just past the extended grapheme cluster of STRING that starts at START,
looking no further than END. START must be a cluster boundary below END."
  (declare (optimize speed))
  (let ((code (char-code (schar string start)))
        (next (1+ start)))
    (declare (type fixnum next))
    (cond ((= next end) end)
          ((and (< code #x80) (< (char-code (schar string next)) #x80))
           ;; Two ASCII characters are always separate clusters, except CR LF.
           (if (and (= code 13) (= (char-code (schar string next)) 10))
               (1+ next)
               next))
          (t (slow-grapheme-end string start end)))))

;;; Grapheme width

(defun regional-indicator-code-p (code)
  (<= #x1F1E6 code #x1F1FF))

(defun cluster-width (string start end)
  (declare (type simple-string string) (type fixnum start end))
  (let* ((base (schar string start))
         (code (char-code base))
         (width (char-width base)))
    (cond ((control-code-p code) 0)
          ((and (regional-indicator-code-p code) (> (- end start) 1)) 2)
          (t
           (loop for index from (1+ start) below end
                 for c = (char-code (schar string index))
                 do (cond ((and (= c #xFE0F) (= width 1) (emoji-p base))
                           ;; VS16 asks for emoji presentation, which is wide.
                           (setf width 2))
                          ((or (= c #xFF9E) (= c #xFF9F))
                           ;; Halfwidth (semi-)voiced sound marks are extending
                           ;; characters but terminals give each its own column.
                           (setf width (min 2 (1+ width))))))
           width))))

(declaim (ftype (function (simple-string fixnum fixnum) (values (integer 0 2) &optional))
                grapheme-width))
(defun grapheme-width (string start end)
  "Return how many columns the grapheme cluster of STRING from START to END occupies: 0
for control characters and lone zero-width characters, otherwise 1 or 2. The width is that
of the first character, made 2 by VS16 (emoji presentation) and for flag pairs."
  (declare (optimize speed))
  (let ((code (char-code (schar string start))))
    (if (and (= end (1+ start)) (< #x1F code #x7F))
        1
        (cluster-width string start end))))

(defun uncertain-width-p (string start end)
  "True when terminals disagree on the width of the grapheme cluster of STRING from START
to END: wide clusters containing VS16 (U+FE0F), which some terminals draw in one column."
  (declare (type simple-string string) (type fixnum start end))
  (and (find (code-char #xFE0F) string :start start :end end)
       (= (grapheme-width string start end) 2)))

;;; Iteration

(defmacro do-graphemes (((start end &optional width) string &key ((:start from) 0) to)
                        &body body)
  "Evaluate BODY once for each grapheme cluster of the simple string STRING between FROM
and TO (default: the whole string), with START and END bound to the cluster's bounds and
WIDTH, if given, to its display width."
  (let ((s (gensym "STRING")) (limit (gensym "LIMIT"))
        (position (gensym "POSITION")) (next (gensym "NEXT")))
    `(let* ((,s ,string)
            (,limit (or ,to (length ,s)))
            (,position ,from)
            (,next 0))
       (declare (type simple-string ,s) (type fixnum ,limit ,position ,next))
       (loop while (< ,position ,limit)
             do (setf ,next (grapheme-end ,s ,position ,limit))
                (let* ((,start ,position)
                       (,end ,next)
                       ,@(when width `((,width (grapheme-width ,s ,start ,end)))))
                  (declare (type fixnum ,start ,end) (ignorable ,start ,end))
                  ,@body)
                (setf ,position ,next)))))

(defun string-width (string &key (start 0) end)
  "Return how many columns STRING occupies from START to END: the sum of the widths of
its grapheme clusters."
  (let ((string (if (simple-string-p string) string (coerce string 'simple-string)))
        (total 0))
    (declare (type fixnum total))
    (do-graphemes ((s e width) string :start start :to end)
      (incf total width))
    total))
