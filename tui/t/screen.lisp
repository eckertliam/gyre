;;;; screen.lisp — a small terminal screen model for testing the renderer.
;;;;
;;;; It interprets exactly the sequences the renderer emits (cursor positioning, SGR,
;;;; erase, cursor visibility and shape, synchronized output) plus UTF-8 text with wide
;;;; graphemes, and signals an error on anything else, so a test fails if the renderer
;;;; starts emitting something unexpected.

(defpackage #:tui/tests/screen
  (:use #:cl)
  (:import-from #:tui/width #:grapheme-end #:grapheme-width #:uncertain-width-p)
  (:import-from #:tui/style
                #:rgb #:+reset+ #:+bold+ #:+dim+ #:+italic+ #:+underline+ #:+double-underline+
                #:+curly-underline+ #:+dotted-underline+ #:+dashed-underline+
                #:+any-underline+ #:+blink+ #:+reverse+ #:+hidden+ #:+strikethrough+
                #:underline-style)
  (:import-from #:tui/buffer
                #:buffer-area #:cell-grapheme #:cell-fg #:cell-bg #:cell-underline-color
                #:cell-modifiers #:cell-continuation-p #:cell-wide-p)
  (:import-from #:tui/layout #:rect-x #:rect-y #:rect-right #:rect-bottom)
  (:export #:screen
           #:make-screen
           #:feed-screen
           #:screen-cursor-x
           #:screen-cursor-y
           #:screen-cursor-visible
           #:screen-cursor-shape
           #:screen-text
           #:screen-cell-description
           #:screen-mismatches
           #:unexpected-output))

(in-package #:tui/tests/screen)

(define-condition unexpected-output (error)
  ((message :initarg :message :reader unexpected-output-message))
  (:report (lambda (c s) (format s "Unexpected terminal output: ~A" (unexpected-output-message c)))))

(defun unexpected (control &rest arguments)
  (error 'unexpected-output :message (apply #'format nil control arguments)))

(defstruct (cell (:conc-name sc-) (:copier nil))
  (grapheme " ")
  (fg +reset+) (bg +reset+) (underline-color +reset+) (modifiers 0)
  (wide nil) (continuation nil))

(defstruct (screen (:constructor %make-screen))
  width height cells
  (cursor-x 0) (cursor-y 0) (pending-wrap nil)
  (fg +reset+) (bg +reset+) (underline-color +reset+) (modifiers 0)
  (cursor-visible t) (cursor-shape 0)
  ;; How many columns this terminal gives a grapheme containing VS16: 2 or 1.
  (vs16-width 2))

(defun make-screen (width height &key (vs16-width 2))
  (let ((screen (%make-screen :width width :height height :vs16-width vs16-width
                              :cells (make-array (list height width)))))
    (erase screen)
    screen))

(defun erase (screen)
  (dotimes (y (screen-height screen))
    (dotimes (x (screen-width screen))
      (setf (aref (screen-cells screen) y x) (make-cell)))))

(defun cell-at (screen x y)
  (aref (screen-cells screen) y x))

;;; Text

(defun blank-keeping-style (screen x y)
  (let ((cell (cell-at screen x y)))
    (setf (sc-grapheme cell) " " (sc-wide cell) nil (sc-continuation cell) nil)))

(defun put-cell (screen x y grapheme wide continuation)
  (setf (aref (screen-cells screen) y x)
        (make-cell :grapheme grapheme :fg (screen-fg screen) :bg (screen-bg screen)
                   :underline-color (screen-underline-color screen)
                   :modifiers (screen-modifiers screen)
                   :wide wide :continuation continuation)))

(defun write-grapheme (screen grapheme)
  (let* ((uncertain (uncertain-width-p grapheme 0 (length grapheme)))
         (width (if uncertain
                    (screen-vs16-width screen)
                    (grapheme-width grapheme 0 (length grapheme)))))
    (when (zerop width) (unexpected "zero-width grapheme ~S" grapheme))
    (when (screen-pending-wrap screen)
      (unexpected "text ~S written in the pending-wrap state at the right edge" grapheme))
    (let ((x (screen-cursor-x screen))
          (y (screen-cursor-y screen)))
      (when (> (+ x width) (screen-width screen))
        (unexpected "wide grapheme ~S at the last column" grapheme))
      ;; Overwriting half of a wide grapheme blanks the other half.
      (when (sc-continuation (cell-at screen x y))
        (blank-keeping-style screen (1- x) y))
      (let ((last (+ x width -1)))
        (when (and (sc-wide (cell-at screen last y)) (< (1+ last) (screen-width screen)))
          (blank-keeping-style screen (1+ last) y)))
      (put-cell screen x y grapheme (= width 2) nil)
      (when (= width 2)
        (put-cell screen (1+ x) y "" nil t))
      (let ((next (+ x width)))
        (if (>= next (screen-width screen))
            (setf (screen-cursor-x screen) (1- (screen-width screen))
                  (screen-pending-wrap screen) t)
            (setf (screen-cursor-x screen) next))))))

(defun write-text (screen string)
  (let ((string (coerce string 'simple-string)))
    (loop with start = 0
          while (< start (length string))
          do (let ((end (grapheme-end string start (length string))))
               (write-grapheme screen (subseq string start end))
               (setf start end)))))

;;; Control sequences

(defun parse-parameters (string)
  "Split STRING into parameters at semicolons and each parameter into sub-parameters at
colons. Empty fields become NIL."
  (flet ((split (string char)
           (loop with start = 0
                 for end = (position char string :start start)
                 collect (subseq string start end)
                 while end do (setf start (1+ end)))))
    (mapcar (lambda (parameter)
              (mapcar (lambda (field) (and (plusp (length field)) (parse-integer field)))
                      (split parameter #\:)))
            (split string #\;))))

(defun extended-color (subs rest)
  "Parse a 38/48/58 color from its colon sub-parameters SUBS or the following parameters
REST. Return the color and the remaining parameters."
  (if (rest subs)
      (ecase (second subs)
        (5 (values (third subs) rest))
        (2 (destructuring-bind (r g b) (last subs 3) (values (rgb r g b) rest))))
      (ecase (first (first rest))
        (5 (values (first (second rest)) (cddr rest)))
        (2 (values (rgb (first (second rest)) (first (third rest)) (first (fourth rest)))
                   (nthcdr 4 rest))))))

(defun set-underline (screen bit)
  (setf (screen-modifiers screen)
        (logior (logandc2 (screen-modifiers screen) +any-underline+) bit)))

(defun sgr-reset (screen)
  (setf (screen-fg screen) +reset+ (screen-bg screen) +reset+
        (screen-underline-color screen) +reset+ (screen-modifiers screen) 0))

(defun apply-sgr (screen parameters)
  (loop while parameters
        do (let* ((subs (pop parameters))
                  (n (or (first subs) 0)))
             (macrolet ((mod-on (bit) `(setf (screen-modifiers screen)
                                             (logior (screen-modifiers screen) ,bit)))
                        (mod-off (bit) `(setf (screen-modifiers screen)
                                              (logandc2 (screen-modifiers screen) ,bit))))
               (cond ((= n 0) (sgr-reset screen))
                     ((= n 1) (mod-on +bold+))
                     ((= n 2) (mod-on +dim+))
                     ((= n 3) (mod-on +italic+))
                     ((= n 4) (set-underline screen
                                             (ecase (or (second subs) 1)
                                               (0 0) (1 +underline+) (2 +double-underline+)
                                               (3 +curly-underline+) (4 +dotted-underline+)
                                               (5 +dashed-underline+))))
                     ((= n 5) (mod-on +blink+))
                     ((= n 7) (mod-on +reverse+))
                     ((= n 8) (mod-on +hidden+))
                     ((= n 9) (mod-on +strikethrough+))
                     ((= n 21) (set-underline screen +double-underline+))
                     ((= n 22) (mod-off (logior +bold+ +dim+)))
                     ((= n 23) (mod-off +italic+))
                     ((= n 24) (mod-off +any-underline+))
                     ((= n 25) (mod-off +blink+))
                     ((= n 27) (mod-off +reverse+))
                     ((= n 28) (mod-off +hidden+))
                     ((= n 29) (mod-off +strikethrough+))
                     ((<= 30 n 37) (setf (screen-fg screen) (- n 30)))
                     ((= n 38) (multiple-value-bind (color rest) (extended-color subs parameters)
                                 (setf (screen-fg screen) color parameters rest)))
                     ((= n 39) (setf (screen-fg screen) +reset+))
                     ((<= 40 n 47) (setf (screen-bg screen) (- n 40)))
                     ((= n 48) (multiple-value-bind (color rest) (extended-color subs parameters)
                                 (setf (screen-bg screen) color parameters rest)))
                     ((= n 49) (setf (screen-bg screen) +reset+))
                     ((= n 58) (multiple-value-bind (color rest) (extended-color subs parameters)
                                 (setf (screen-underline-color screen) color parameters rest)))
                     ((= n 59) (setf (screen-underline-color screen) +reset+))
                     ((<= 90 n 97) (setf (screen-fg screen) (+ 8 (- n 90))))
                     ((<= 100 n 107) (setf (screen-bg screen) (+ 8 (- n 100))))
                     (t (unexpected "SGR parameter ~D" n)))))))

(defun first-parameter (parameters default)
  (or (first (first parameters)) default))

(defun apply-csi (screen private parameters intermediates final)
  (let ((parameters (parse-parameters parameters)))
    (cond ((and (not private) (string= intermediates "") (char= final #\H))
           (setf (screen-cursor-y screen) (1- (first-parameter parameters 1))
                 (screen-cursor-x screen) (1- (or (first (second parameters)) 1))
                 (screen-pending-wrap screen) nil)
           (unless (and (< -1 (screen-cursor-x screen) (screen-width screen))
                        (< -1 (screen-cursor-y screen) (screen-height screen)))
             (unexpected "cursor moved off screen")))
          ((and (not private) (string= intermediates "") (char= final #\m))
           (apply-sgr screen parameters))
          ((and (not private) (char= final #\J) (= 2 (first-parameter parameters 0)))
           (unless (and (= (screen-bg screen) +reset+) (zerop (screen-modifiers screen)))
             (unexpected "erase with non-default attributes"))
           (erase screen))
          ((and (not private) (string= intermediates " ") (char= final #\q))
           (setf (screen-cursor-shape screen) (first-parameter parameters 0)))
          ((and private (member final '(#\h #\l)))
           (let ((mode (first-parameter parameters 0)))
             (case mode
               (25 (setf (screen-cursor-visible screen) (char= final #\h)))
               ((2026 1049 2004 1004 1000 1002 1003 1006))
               (t (unexpected "private mode ~D" mode)))))
          ((and private (char= final #\u)))      ; kitty keyboard push and pop
          (t (unexpected "CSI ~:[~;?~]~A~A~A" private parameters intermediates final)))))

(defun feed-screen (screen octets)
  "Interpret OCTETS, as written by the renderer, on SCREEN."
  (let ((string (sb-ext:octets-to-string (coerce octets '(vector (unsigned-byte 8)))
                                         :external-format :utf-8))
        (text (make-string-output-stream)))
    (flet ((flush-text ()
             (let ((pending (get-output-stream-string text)))
               (when (plusp (length pending))
                 (write-text screen pending)))))
      (loop with i = 0
            while (< i (length string))
            do (let ((char (char string i)))
                 (cond ((char/= char (code-char 27))
                        (when (< (char-code char) 32)
                          (unexpected "control character ~D" (char-code char)))
                        (write-char char text)
                        (incf i))
                       ((and (< (1+ i) (length string)) (char= (char string (1+ i)) #\[))
                        (flush-text)
                        (let* ((start (+ i 2))
                               (private (and (< start (length string))
                                             (member (char string start) '(#\? #\> #\<))))
                               (params-start (if private (1+ start) start))
                               (final (position-if (lambda (c) (<= #x40 (char-code c) #x7E))
                                                   string :start params-start)))
                          (unless final (unexpected "unterminated CSI"))
                          (let* ((body (subseq string params-start final))
                                 (split (or (position-if (lambda (c) (<= #x20 (char-code c) #x2F))
                                                         body)
                                            (length body))))
                            (apply-csi screen private (subseq body 0 split) (subseq body split)
                                       (char string final)))
                          (setf i (1+ final))))
                       (t (unexpected "ESC not followed by ["))))
            finally (flush-text)))
    screen))

;;; Inspecting

(defun screen-cell-description (screen x y)
  "The grapheme and attributes of cell (X, Y) of SCREEN as a list."
  (let ((cell (cell-at screen x y)))
    (list (sc-grapheme cell) (sc-fg cell) (sc-bg cell) (sc-underline-color cell)
          (sc-modifiers cell) (sc-continuation cell))))

(defun screen-text (screen)
  "The graphemes on SCREEN as a list of strings, one per row."
  (loop for y below (screen-height screen)
        collect (with-output-to-string (out)
                  (dotimes (x (screen-width screen))
                    (write-string (sc-grapheme (cell-at screen x y)) out)))))

(defun same-modifiers-p (a b)
  "True when modifier sets A and B look the same: the same non-underline modifiers and the
same visible underline style."
  (and (= (logandc2 a +any-underline+) (logandc2 b +any-underline+))
       (eq (underline-style a) (underline-style b))))

(defun cell-matches-p (cell grapheme fg bg underline-color modifiers)
  (and (string= (sc-grapheme cell) grapheme)
       (= (sc-fg cell) fg) (= (sc-bg cell) bg)
       (= (sc-underline-color cell) underline-color)
       (same-modifiers-p (sc-modifiers cell) modifiers)))

(defun screen-mismatches (screen buffer &key ignore)
  "Return a list describing each cell where SCREEN doesn't show what BUFFER holds, skipping
cells for which IGNORE (a function of x and y) returns true. A continuation cell matches the
right half of a wide grapheme, or a space in the same style when the terminal drew the
grapheme narrow."
  (let ((area (buffer-area buffer))
        (mismatches '()))
    (loop for y from (rect-y area) below (rect-bottom area)
          do (loop for x from (rect-x area) below (rect-right area)
                   unless (and ignore (funcall ignore x y))
                     do (let ((cell (cell-at screen x y))
                              (fg (cell-fg buffer x y)) (bg (cell-bg buffer x y))
                              (underline (cell-underline-color buffer x y))
                              (modifiers (cell-modifiers buffer x y)))
                          (unless (if (cell-continuation-p buffer x y)
                                      (or (and (sc-continuation cell)
                                               (cell-matches-p cell "" fg bg underline modifiers))
                                          (and (= 1 (screen-vs16-width screen))
                                               (cell-matches-p cell " " fg bg underline
                                                               modifiers)))
                                      (cell-matches-p cell (cell-grapheme buffer x y)
                                                      fg bg underline modifiers))
                            (push (list x y :expected (cell-grapheme buffer x y) fg bg underline
                                        modifiers
                                        :got (sc-grapheme cell) (sc-fg cell) (sc-bg cell)
                                        (sc-underline-color cell) (sc-modifiers cell))
                                  mismatches)))))
    (nreverse mismatches)))
