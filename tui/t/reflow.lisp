;;;; reflow.lisp — wrapping checked against a literal port of ratatui's WordWrapper
;;;; (ratatui-widgets/src/reflow.rs), on hand-picked and random lines.

(defpackage #:tui/tests/reflow
  (:use #:cl)
  (:import-from #:tui/tests/harness #:deftest #:is)
  (:import-from #:tui/width #:do-graphemes)
  (:import-from #:tui/text #:span-content #:make-line #:line-spans #:make-text #:text-lines)
  (:import-from #:tui/text/wrap #:map-wrapped-lines))

(in-package #:tui/tests/reflow)

(defun str (&rest parts)
  "Build a string from characters, code points and strings."
  (with-output-to-string (out)
    (dolist (part parts)
      (etypecase part
        (character (write-char part out))
        (integer (write-char (code-char part) out))
        (string (write-string part out))))))

(defun contents (line)
  (map 'list #'span-content (line-spans line)))

;;; A literal port of ratatui's WordWrapper::process_input on lists of graphemes. With FIXES
;;; it makes the same three fixes as tui/text/wrap; without, it reproduces ratatui.

(defun reference-wrap (graphemes max-width &key (trim t) (fixes t))
  "The rows of GRAPHEMES, a list of (WIDTH WHITESPACEP . ID), as lists of graphemes."
  (let ((rows '()) (row '()) (row-width 0) (word '()) (word-width 0)
        (spaces '()) (spaces-width 0) (after-word nil))
    (labels ((flush ()
               (when (or row (not trim))
                 (setf row (append row spaces))
                 (incf row-width spaces-width))
               (setf row (append row word))
               (incf row-width word-width)
               (setf spaces '() spaces-width 0 word '() word-width 0))
             (emit ()
               (push row rows)
               (setf row '() row-width 0))
             (drop-spaces (remaining)
               (loop while (and spaces (<= (first (first spaces)) remaining))
                     do (decf remaining (first (first spaces)))
                        (decf spaces-width (first (pop spaces)))))
             (row-full-p (width)
               (or (>= row-width max-width)
                   (and (> width 0)
                        (if fixes
                            (> (+ row-width (if (and (null row) trim) 0 spaces-width)
                                  word-width width)
                               max-width)
                            (>= (+ row-width spaces-width word-width) max-width)))))
             (process (grapheme width whitespacep)
               (when (or (and after-word whitespacep)
                         (and (null row) trim (> (+ word-width width) max-width))
                         (and (null row) trim (> (+ spaces-width width) max-width))
                         (and (null row) (not trim)
                              (> (+ word-width spaces-width width) max-width)))
                 (flush))
               (when (row-full-p width)
                 (let ((remaining (max 0 (- max-width row-width))))
                   (emit)
                   (drop-spaces remaining))
                 (when (and whitespacep (null spaces))
                   (return-from process))
                 (when (and fixes (not whitespacep) (> width 0)
                            (> (+ (if trim 0 spaces-width) word-width width) max-width))
                   (flush)
                   (emit)))
               (if whitespacep
                   (setf spaces (append spaces (list grapheme))
                         spaces-width (+ spaces-width width))
                   (setf word (append word (list grapheme))
                         word-width (+ word-width width)))
               (setf after-word (not whitespacep))))
      (dolist (grapheme graphemes)
        (destructuring-bind (width whitespacep . id) grapheme
          (declare (ignore id))
          (unless (> width max-width)
            (process grapheme width whitespacep))))
      (when (and (null row) (null word) spaces trim)
        (push '() rows))
      (when (or row (not trim))
        (setf row (append row spaces)))
      (setf row (append row word))
      (when row
        (push row rows))
      (unless rows
        (push '() rows))
      (nreverse rows))))

(defun control-cluster-p (string start end)
  (loop for index from start below end
        thereis (let ((code (char-code (char string index))))
                  (or (< code 32) (<= 127 code 159)))))

(defun whitespace-cluster-p (string start end)
  ;; Ratatui's StyledGrapheme::is_whitespace, also excluding U+2007 and U+202F.
  (let ((cluster (subseq string start end)))
    (or (string= cluster (str #x200B))
        (and (every #'sb-unicode:whitespace-p cluster)
             (notany (lambda (char) (member (char-code char) '(#xA0 #x2007 #x202F)))
                     cluster)))))

(defun line-graphemes (line)
  "The graphemes ratatui wraps for LINE: every cluster but control characters, as
(WIDTH WHITESPACEP SPAN START END)."
  (let ((graphemes '()))
    (loop for span across (line-spans line)
          for index from 0
          for content = (span-content span)
          do (do-graphemes ((start end width) content)
               (unless (control-cluster-p content start end)
                 (push (list* width (whitespace-cluster-p content start end)
                              (list index start end))
                       graphemes))))
    (nreverse graphemes)))

(defun reference-rows (string width &key (trim t) (fixes t))
  "The rows of the text STRING as REFERENCE-WRAP wraps them, as strings."
  (loop for line across (text-lines (make-text string))
        nconc (mapcar (lambda (row)
                        (apply #'concatenate 'string
                               (mapcar (lambda (grapheme)
                                         (destructuring-bind (span start end) (cddr grapheme)
                                           (subseq (span-content (svref (line-spans line) span))
                                                   start end)))
                                       row)))
                      (reference-wrap (line-graphemes line) width :trim trim :fixes fixes))))

(deftest the-reference-reproduces-ratatui
  (is (equal '("abcd efghij" "klmnopabcd efgh" "ijklmnopabcdefg" "hijkl mnopab c d e f"
               "g h i j k l m n o")
             (reference-rows (format nil "abcd efghij klmnopabcd efgh ijklmnopabcdefg ~
                                          hijkl mnopab c d e f g h i j k l m n o")
                             20 :fixes nil)))
  (is (equal '("          " "    4" "Indent")
             (reference-rows "               4 Indent" 10 :trim nil :fixes nil)))
  ;; The three faults that tui/text/wrap fixes.
  (is (equal '("日本") (reference-rows "日本" 3 :fixes nil)))
  (is (equal '("abcd" "" "efg" "h") (reference-rows "abcd   efg h" 4 :fixes nil)))
  (is (equal (list (str #x2060) "abcd")
             (reference-rows (str #x2060 #x200B "abcd") 3 :fixes nil))))

;;; Random lines

(defparameter *pieces*
  (list "a" "b" "cd" "efg" "hijklmn" " " " " "  " "    " "日" "本語" (str #x1F600)
        (str #x2764 #xFE0F) (str "e" #x301) (str #x301) (str #xA0) (str #x200B) (str #x3000)
        (str #x2060) (str 9) (str 13 10) (str #x1F1FA #x1F1F8) "-" "x")
  "Pieces of random lines: words, spaces, wide and zero-width clusters, no-break and wide
spaces, control characters.")

(defun random-line (state)
  (make-line (loop repeat (random 4 state)
                   collect (apply #'concatenate 'string
                                  (loop repeat (random 7 state)
                                        collect (nth (random (length *pieces*) state)
                                                     *pieces*))))))

(defun row-clusters (line row)
  "The clusters of LINE in ROW, a list of the arguments MAP-WRAPPED-LINES passed, as
(WIDTH CONTROLP SPAN START END)."
  (destructuring-bind (start-span start-char end-span end-char width) row
    (declare (ignore width))
    (let ((spans (line-spans line))
          (clusters '()))
      (loop for index from start-span to (min end-span (1- (length spans)))
            for content = (span-content (svref spans index))
            do (do-graphemes ((start end width) content
                              :start (if (= index start-span) start-char 0)
                              :to (if (= index end-span) end-char nil))
                 (push (list width (control-cluster-p content start end) index start end)
                       clusters)))
      (nreverse clusters))))

(defun offset (line span char)
  "Position (SPAN, CHAR) of LINE as a character offset from the line's start."
  (+ char (loop for index below span
                sum (length (span-content (svref (line-spans line) index))))))

(defun rows-in-order-p (line rows)
  (loop with previous-end = 0
        for (start-span start-char end-span end-char) in rows
        for start = (offset line start-span start-char)
        for end = (offset line end-span end-char)
        always (<= previous-end start end)
        do (setf previous-end end)))

(defun ids (clusters width)
  "The positions of CLUSTERS, lists (WIDTH ... SPAN START END), that a row must show: all of
them, or at width 1 those with a width."
  (mapcar (lambda (cluster) (last cluster 3))
          (if (= width 1) (remove 0 clusters :key #'first) clusters)))

(defun wrapping-problem (line width trim)
  "How wrapping LINE to WIDTH disagrees with the reference or breaks a rule, or NIL."
  (let* ((rows (let ((rows '()))
                 (map-wrapped-lines (lambda (&rest row) (push row rows)) line width :trim trim)
                 (nreverse rows)))
         (clusters (mapcar (lambda (row) (remove-if #'second (row-clusters line row))) rows))
         (expected (reference-wrap (line-graphemes line) width :trim trim)))
    (cond ((/= (length rows) (length expected)) :row-count)
          ((notevery (lambda (row) (<= (fifth row) width)) rows) :too-wide)
          ((notevery (lambda (row row-clusters)
                       (= (fifth row) (reduce #'+ row-clusters :key #'first)))
                     rows clusters)
           :width)
          ((not (rows-in-order-p line rows)) :order)
          ((notevery (lambda (row-clusters reference)
                       (equal (ids row-clusters width)
                              (ids (mapcar (lambda (g) (list* (first g) (cddr g))) reference)
                                   width)))
                     clusters expected)
           :content))))

(deftest wrapping-random-lines-matches-the-reference
  (let ((state (sb-ext:seed-random-state 1234))
        (problems '()))
    (dotimes (i 1500)
      (let ((line (random-line state)))
        (loop for width from 1 to 10
              do (dolist (trim '(t nil))
                   (let ((problem (wrapping-problem line width trim)))
                     (when problem
                       (push (list problem (contents line) width trim) problems)))))))
    (is (null problems))))
