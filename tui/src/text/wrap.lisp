;;;; wrap.lisp — word wrapping a line into rows no wider than a number of columns.
;;;;
;;;; The algorithm is ratatui's WordWrapper (ratatui-widgets/src/reflow.rs). It keeps the
;;;; pending row, whitespace and word as ranges of the line instead of vectors of graphemes,
;;;; so that wrapping doesn't allocate. Three fixes, marked "Fix:", keep every row within the
;;;; width and stop empty rows from appearing in the middle of a line.

(defpackage #:tui/text/wrap
  (:use #:cl)
  (:import-from #:tui/style #:patch-style)
  (:import-from #:tui/width #:grapheme-end #:grapheme-width #:do-graphemes)
  (:import-from #:tui/text
                #:line #:line-spans #:line-style #:line-alignment #:span-content
                #:text-lines #:text-style #:text-alignment)
  (:import-from #:tui/text/range #:copy-line-range)
  (:export #:map-wrapped-lines
           #:wrap-line
           #:wrap-text))

(in-package #:tui/text/wrap)

;;; Clusters

(declaim (inline control-cluster-p))
(defun control-cluster-p (content start)
  "True when the cluster at START of CONTENT is a C0 or C1 control character (or CR LF).
Ratatui filters these out before wrapping."
  (declare (type simple-string content) (type fixnum start))
  (let ((code (char-code (schar content start))))
    (or (< code #x20) (<= #x7F code #x9F))))

(defun separator-char-p (char)
  ;; Unicode White_Space except the no-break spaces, plus ZERO WIDTH SPACE.
  (let ((code (char-code char)))
    (cond ((= code #x200B) t)
          ((or (= code #xA0) (= code #x2007) (= code #x202F)) nil)
          (t (and (sb-unicode:whitespace-p char) t)))))

(defun whitespace-cluster-p (content start end)
  "True when the cluster of CONTENT from START to END separates words."
  (declare (type simple-string content) (type fixnum start end))
  (let ((char (schar content start)))
    (if (< (char-code char) #x80)
        (and (char= char #\Space) (= end (1+ start)))
        (loop for index of-type fixnum from start below end
              always (separator-char-p (schar content index))))))

(defun counted-width (content start end max-width)
  "The columns the wrapper counts for the cluster of CONTENT from START to END: none for
the clusters it ignores."
  (declare (type simple-string content) (type fixnum start end max-width))
  (let ((width (grapheme-width content start end)))
    (if (or (control-cluster-p content start) (> width max-width)) 0 width)))

(defun next-cluster (spans span char)
  "The position of the first cluster at or after CHAR in span SPAN of SPANS. There must be
one."
  (declare (type simple-vector spans) (type fixnum span char))
  (loop while (>= char (length (span-content (svref spans span))))
        do (incf span)
           (setf char 0))
  (values span char))

;;; Pending text

(declaim (inline make-segment))
(defstruct (segment (:constructor make-segment ())
                    (:copier nil)
                    (:predicate nil))
  "Consecutive grapheme clusters of a line, from the start of the first to the end of the
last, and their width. Clusters the wrapper ignores can lie in between."
  (emptyp t :type boolean)
  (start-span 0 :type fixnum)
  (start-char 0 :type fixnum)
  (end-span 0 :type fixnum)
  (end-char 0 :type fixnum)
  (width 0 :type fixnum))

(declaim (inline clear-segment))
(defun clear-segment (segment)
  (setf (segment-emptyp segment) t
        (segment-width segment) 0))

(defun add-cluster (segment span start end width)
  "Append the cluster from START to END of span SPAN, WIDTH columns wide, to SEGMENT."
  (declare (type segment segment) (type fixnum span start end width))
  (when (segment-emptyp segment)
    (setf (segment-emptyp segment) nil
          (segment-start-span segment) span
          (segment-start-char segment) start))
  (setf (segment-end-span segment) span
        (segment-end-char segment) end)
  (incf (segment-width segment) width))

(defun add-segment (row segment)
  "Append SEGMENT, which comes after ROW in the line, to ROW."
  (declare (type segment row segment))
  (unless (segment-emptyp segment)
    (when (segment-emptyp row)
      (setf (segment-emptyp row) nil
            (segment-start-span row) (segment-start-span segment)
            (segment-start-char row) (segment-start-char segment)))
    (setf (segment-end-span row) (segment-end-span segment)
          (segment-end-char row) (segment-end-char segment))
    (incf (segment-width row) (segment-width segment))))

(declaim (inline make-wrapper))
(defstruct (wrapper (:constructor make-wrapper
                        (line function max-width trim row spaces word))
                    (:copier nil)
                    (:predicate nil))
  "The state of wrapping one line: ratatui's WordWrapper, with its pending line, whitespace
and word as the segments ROW, SPACES and WORD."
  (line nil :type line :read-only t)
  (function nil :type function :read-only t)
  (max-width 1 :type (and fixnum (integer 1)) :read-only t)
  (trim t :type boolean :read-only t)
  (row nil :type segment :read-only t)
  (spaces nil :type segment :read-only t)
  (word nil :type segment :read-only t)
  (after-word-p nil :type boolean)      ; the last cluster added was not whitespace
  (rows 0 :type fixnum))

;;; Reporting rows

(defun emit (state start-span start-char end-span end-char width)
  (declare (type wrapper state))
  (incf (wrapper-rows state))
  (funcall (wrapper-function state) start-span start-char end-span end-char width))

(defun emit-empty (state span char)
  (emit state span char span char 0))

(defun showable-bounds (spans row)
  "The bounds of ROW for a width of 1, without the wide clusters inside it, which can't be
shown: the run of clusters around the row's visible cluster that contains no wide one.
Return its start span, start char, end span and end char."
  (declare (type simple-vector spans) (type segment row))
  (let ((row-start (segment-start-span row))
        (row-end (segment-end-span row))
        (first-span -1) (first-char 0) (last-span 0) (last-char 0)
        (visiblep nil))
    (declare (type fixnum first-span first-char last-span last-char))
    (block walk
      (loop for index of-type fixnum from row-start to row-end
            for content = (span-content (svref spans index))
            do (do-graphemes ((start end width) content
                              :start (if (= index row-start) (segment-start-char row) 0)
                              :to (if (= index row-end) (segment-end-char row) nil))
                 (cond ((> width 1)
                        (when visiblep (return-from walk))
                        (setf first-span -1))
                       (t
                        (when (< first-span 0)
                          (setf first-span index first-char start))
                        (setf last-span index last-char end)
                        (when (> width 0) (setf visiblep t)))))))
    (if (< first-span 0)
        (values row-start (segment-start-char row) row-start (segment-start-char row))
        (values first-span first-char last-span last-char))))

(defun emit-row (state)
  "Report the pending row and empty it."
  (declare (type wrapper state))
  (let ((row (wrapper-row state)))
    (if (= (wrapper-max-width state) 1)
        (multiple-value-bind (start-span start-char end-span end-char)
            (showable-bounds (line-spans (wrapper-line state)) row)
          (emit state start-span start-char end-span end-char (segment-width row)))
        (emit state (segment-start-span row) (segment-start-char row)
              (segment-end-span row) (segment-end-char row) (segment-width row)))
    (clear-segment row)))

(defun drop-spaces (state columns)
  "Drop pending whitespace clusters from the front while they fit in COLUMNS columns."
  (declare (type wrapper state) (type fixnum columns))
  (let ((spaces (wrapper-spaces state))
        (spans (line-spans (wrapper-line state)))
        (max-width (wrapper-max-width state)))
    (loop until (segment-emptyp spaces)
          do (let* ((span (segment-start-span spaces))
                    (start (segment-start-char spaces))
                    (content (span-content (svref spans span)))
                    (end (grapheme-end content start (length content)))
                    (width (counted-width content start end max-width)))
               (when (> width columns)
                 (return))
               (decf columns width)
               (decf (segment-width spaces) width)
               (if (and (= span (segment-end-span spaces)) (= end (segment-end-char spaces)))
                   (clear-segment spaces)
                   (multiple-value-bind (next-span next-char) (next-cluster spans span end)
                     (setf (segment-start-span spaces) next-span
                           (segment-start-char spaces) next-char)))))))

(defun break-row (state)
  "End the row in the middle of the line, then drop as much of the pending whitespace as fits
in the columns the row leaves unused."
  (let ((unused (max 0 (- (wrapper-max-width state) (segment-width (wrapper-row state))))))
    (emit-row state)
    (drop-spaces state unused)))

;;; Wrapping

(defun flush-pending (state)
  "Move the pending whitespace and word onto the row. When trimming, whitespace at the start
of a row is dropped instead."
  (let ((row (wrapper-row state))
        (spaces (wrapper-spaces state))
        (word (wrapper-word state)))
    (when (or (not (segment-emptyp row)) (not (wrapper-trim state)))
      (add-segment row spaces))
    (add-segment row word)
    (clear-segment spaces)
    (clear-segment word)))

(defun overflow-p (state width)
  "True when the row is empty and the pending text can't take a cluster WIDTH columns wide:
ratatui's trimmed, whitespace and untrimmed overflow, which move the pending text onto the
row."
  (let ((max-width (wrapper-max-width state))
        (spaces (segment-width (wrapper-spaces state)))
        (word (segment-width (wrapper-word state))))
    (and (segment-emptyp (wrapper-row state))
         (if (wrapper-trim state)
             (or (> (+ word width) max-width) (> (+ spaces width) max-width))
             (> (+ spaces word width) max-width)))))

(defun row-full-p (state width)
  "True when the row has to end before a cluster WIDTH columns wide."
  (let* ((row (wrapper-row state))
         (max-width (wrapper-max-width state))
         ;; Fix: leading whitespace that trimming will drop doesn't count. Ratatui counts it
         ;; and emits an empty row when it and the next word don't fit together.
         (spaces (if (and (segment-emptyp row) (wrapper-trim state))
                     0
                     (segment-width (wrapper-spaces state)))))
    (or (>= (segment-width row) max-width)
        ;; Fix: the cluster's own width counts. Ratatui only asks whether the pending text
        ;; fills the row, so a wide cluster could make the row one column too wide.
        (and (> width 0)
             (> (+ (segment-width row) spaces (segment-width (wrapper-word state)) width)
                max-width)))))

(defun word-overflow-p (state width)
  "True when the pending word can't take a cluster WIDTH columns wide even on a row of its
own (with the pending whitespace when not trimming)."
  (let ((word (segment-width (wrapper-word state)))
        (spaces (if (wrapper-trim state) 0 (segment-width (wrapper-spaces state)))))
    (and (> width 0)
         (> (+ spaces word width) (wrapper-max-width state)))))

(defun wrap-cluster (state span start end width whitespacep)
  "Wrap the cluster from START to END of span SPAN, WIDTH columns wide: the body of the loop
in ratatui's WordWrapper::process_input."
  (declare (type wrapper state) (type fixnum span start end width))
  (when (or (and whitespacep (wrapper-after-word-p state)) (overflow-p state width))
    (flush-pending state))
  (when (row-full-p state width)
    (break-row state)
    (when (and whitespacep (segment-emptyp (wrapper-spaces state)))
      ;; Whitespace right at a break is dropped.
      (return-from wrap-cluster))
    ;; Fix: a word that can't take the cluster even on a row of its own becomes that row.
    ;; Ratatui lets the word grow past the width when the row before it had no width.
    (when (and (not whitespacep) (word-overflow-p state width))
      (flush-pending state)
      (emit-row state)))
  (add-cluster (if whitespacep (wrapper-spaces state) (wrapper-word state))
               span start end width)
  (setf (wrapper-after-word-p state) (not whitespacep)))

(defun finish-wrapping (state)
  "Report the rest of the line, and an empty row when the line produced none."
  (let ((row (wrapper-row state))
        (spaces (wrapper-spaces state))
        (word (wrapper-word state)))
    (when (and (wrapper-trim state) (segment-emptyp row) (segment-emptyp word)
               (not (segment-emptyp spaces)))
      ;; Trailing whitespace on a row of its own becomes an empty row, as in ratatui.
      (emit-empty state (segment-start-span spaces) (segment-start-char spaces)))
    (flush-pending state)
    (unless (segment-emptyp row)
      (emit-row state))
    (when (zerop (wrapper-rows state))
      (emit-empty state 0 0))))

(defun wrap-clusters (state)
  (let ((spans (line-spans (wrapper-line state)))
        (max-width (wrapper-max-width state)))
    (dotimes (index (length spans))
      (let ((content (span-content (svref spans index))))
        (do-graphemes ((start end width) content)
          ;; Control characters, and clusters too wide for any row, are ignored.
          (unless (or (control-cluster-p content start) (> width max-width))
            (wrap-cluster state index start end width
                          (whitespace-cluster-p content start end))))))))

(declaim (ftype (function (t line fixnum &key (:trim t)) (values fixnum &optional))
                map-wrapped-lines))
(defun map-wrapped-lines (function line width &key (trim t))
  "Split LINE into rows at most WIDTH columns wide and call FUNCTION with (START-SPAN
START-CHAR END-SPAN END-CHAR ROW-WIDTH) for each, in order. Return the number of rows.

A row is the part of LINE from position (START-SPAN, START-CHAR) up to, not including,
(END-SPAN, END-CHAR), as MAP-LINE-RANGE delimits it, and ROW-WIDTH is its width in columns,
never more than WIDTH. Rows break between words, at whitespace: Unicode White_Space other
than the no-break spaces U+00A0, U+2007 and U+202F, plus U+200B ZERO WIDTH SPACE. A word
wider than WIDTH is broken between grapheme clusters; a cluster never straddles two rows. At
a break, pending whitespace is dropped from the front as far as it fits in the columns the
finished row leaves unused, and a whitespace cluster that causes the break is dropped too
when none is left. With TRIM (the default) whitespace that would start a row is dropped as
well; without, it indents the row. These are the rules of ratatui's WordWrapper.

Every line yields at least one row; a row can be empty (its start and end positions equal),
as for an empty line. A WIDTH below 1 yields no rows. Control characters are ignored: they
take no columns and don't separate words. A cluster wider than WIDTH (a wide one when WIDTH
is 1) can't be shown and is left out of the rows, along with zero-width clusters that would
separate it from a row's visible cluster.

Doesn't allocate unless FUNCTION does, so pass a DYNAMIC-EXTENT closure."
  (if (< width 1)
      0
      (let* ((row (make-segment))
             (spaces (make-segment))
             (word (make-segment))
             (state (make-wrapper line (coerce function 'function) width (and trim t)
                                  row spaces word)))
        (declare (dynamic-extent row spaces word state))
        (wrap-clusters state)
        (finish-wrapping state)
        (wrapper-rows state))))

;;; Wrapping into new lines

(defun wrapped-rows (line width trim style alignment)
  (let ((rows '()))
    (flet ((collect (start-span start-char end-span end-char row-width)
             (declare (ignore row-width))
             (push (copy-line-range line start-span start-char end-span end-char
                                    :style style :alignment alignment)
                   rows)))
      (map-wrapped-lines #'collect line width :trim trim))
    (nreverse rows)))

(defun wrap-line (line width &key (trim t))
  "Return the rows of LINE wrapped to WIDTH columns, as MAP-WRAPPED-LINES splits it, as a
list of fresh lines. Each row keeps its spans' styles and has LINE's style and alignment."
  (wrapped-rows line width trim (line-style line) (line-alignment line)))

(defun wrap-text (text width &key (trim t))
  "Return the rows of every line of TEXT wrapped to WIDTH columns, as MAP-WRAPPED-LINES splits
them, as one list of fresh lines. Each row keeps its spans' styles, and gets its line's style
patched over TEXT's and its line's alignment or else TEXT's, so the rows look like TEXT."
  (loop for line across (text-lines text)
        nconc (wrapped-rows line width trim
                            (patch-style (text-style text) (line-style line))
                            (or (line-alignment line) (text-alignment text)))))
