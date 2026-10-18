;;;; input.lisp — input decoder tests: helpers and decoder tables (used by the other
;;;; input-*.lisp test files), Esc handling, flushing, equivalence and descriptions.

(defpackage #:tui/tests/input
  (:use #:cl)
  (:import-from #:tui/tests/harness #:deftest #:is #:signals)
  (:import-from #:tui/input
                #:make-decoder #:feed-decoder #:flush-decoder #:decoder-pending-p
                #:make-key-event #:make-mouse-event #:make-paste-event #:make-focus-event
                #:make-unknown-event #:make-resize-event #:make-kitty-keyboard-reply
                #:make-mode-report #:make-device-attributes-reply
                #:key-event-kind #:key-event-keypad #:key-event-shifted-key
                #:key-event-base-layout-key #:key-event-text #:key-event-modifiers
                #:key-event= #:describe-event
                #:+shift+ #:+alt+ #:+ctrl+ #:+super+ #:+hyper+ #:+meta+ #:+caps-lock+
                #:+num-lock+)
  (:export #:bytes #:key #:mouse #:paste #:unknown #:row #:row-flushed
           #:define-decoder-table #:decode #:idle-rows #:row-input #:row-events
           #:no-difference #:first-difference #:at-most))

(in-package #:tui/tests/input)

;;; Building input and expected events

(defun bytes (&rest parts)
  "An octet vector of PARTS: integers are bytes, :ESC is 27 and strings are
encoded as UTF-8."
  (let ((list (loop for part in parts
                    append (etypecase part
                             ((unsigned-byte 8) (list part))
                             ((eql :esc) (list 27))
                             (string (coerce (sb-ext:string-to-octets part :external-format :utf-8)
                                             'list))))))
    (make-array (length list) :element-type '(unsigned-byte 8) :initial-contents list)))

(defun modifier-bit (name)
  (ecase name
    (:shift +shift+) (:alt +alt+) (:ctrl +ctrl+) (:super +super+) (:hyper +hyper+)
    (:meta +meta+) (:caps-lock +caps-lock+) (:num-lock +num-lock+)))

(defun key (key &rest spec)
  "A key event for KEY. SPEC holds modifier names (:shift :alt :ctrl ...), :REPEAT
or :RELEASE, :KEYPAD, and :SHIFTED, :BASE or :TEXT followed by a value."
  (let ((event (make-key-event :key key)))
    (loop while spec
          do (let ((item (pop spec)))
               (case item
                 ((:repeat :release) (setf (key-event-kind event) item))
                 (:keypad (setf (key-event-keypad event) t))
                 (:shifted (setf (key-event-shifted-key event) (pop spec)))
                 (:base (setf (key-event-base-layout-key event) (pop spec)))
                 (:text (setf (key-event-text event) (pop spec)))
                 (t (setf (key-event-modifiers event)
                          (logior (modifier-bit item) (key-event-modifiers event)))))))
    event))

(defun mouse (kind button x y &rest modifiers)
  (make-mouse-event :kind kind :button button :x x :y y
                    :modifiers (reduce #'logior (mapcar #'modifier-bit modifiers)
                                       :initial-value 0)))

(defun paste (text)
  (make-paste-event :text text))

(defun unknown (&rest parts)
  (make-unknown-event :bytes (apply #'bytes parts)))

;;; Decoding

(defun decode (input &key splits flush copy)
  "Feed INPUT to a fresh decoder in pieces ending at the indices SPLITS (each
piece copied into its own vector when COPY), then flush it when FLUSH. Return
the events and whether the decoder was pending before the flush."
  (let ((decoder (make-decoder))
        (events '())
        (start 0))
    (dolist (end (append splits (list (length input))))
      (setf events (append events
                           (if copy
                               (feed-decoder decoder (subseq input start end))
                               (feed-decoder decoder input :start start :end end))))
      (setf start end))
    (let ((pending (decoder-pending-p decoder)))
      (when flush
        (setf events (append events (flush-decoder decoder))))
      (values events pending))))

;;; Predicates for IS, defined at compile time too so that IS treats calls to
;;; them as function calls and shows their arguments when an assertion fails.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun decodes-to (input expected actual)
    (declare (ignore input))
    (equalp expected actual))

  (defun pending-as-expected (input flush pending)
    (declare (ignore input))
    (eq (not flush) (not pending)))

  (defun same-when-split (input mismatch)
    (declare (ignore input))
    (null mismatch))

  (defun same-events (legacy kitty legacy-events kitty-events)
    (declare (ignore legacy kitty))
    (and legacy-events (equalp legacy-events kitty-events)))

  (defun same-keys (legacy kitty legacy-events kitty-events)
    (declare (ignore legacy kitty))
    (and (= 1 (length legacy-events) (length kitty-events))
         (key-event= (first legacy-events) (first kitty-events))))

  (defun no-difference (round difference)
    (declare (ignore round))
    (null difference))

  (defun at-most (limit value)
    (<= value limit)))

(defun first-difference (expected actual)
  "Where the event lists EXPECTED and ACTUAL first differ, or nil."
  (let ((index (mismatch expected actual :test #'equalp)))
    (when index
      (list :index index :expected (nth index expected) :actual (nth index actual)))))

;;; Decoder tables. A row is input bytes and the events they must decode to,
;;; optionally after a flush. Every row is also checked split at every position
;;; and fed one byte at a time.

(defstruct (row (:constructor make-row (input events flush)))
  input events flush)

(defun row (input &rest events)
  "A table row: INPUT decodes to EVENTS and leaves nothing pending."
  (make-row input events nil))

(defun row-flushed (input &rest events)
  "A table row: INPUT leaves bytes pending, and decodes to EVENTS once flushed."
  (make-row input events t))

(defvar *tables* (make-hash-table :test 'eq)
  "Functions returning the rows of each decoder table, keyed by table name.")

(defmacro define-decoder-table (name &body rows)
  "Define the decoder table NAME and a test of the same name that checks it.
Each of ROWS evaluates to a row or a list of rows."
  `(progn
     (setf (gethash ',name *tables*)
           (lambda () (loop for row in (list ,@rows)
                            if (listp row) append row else collect row)))
     (deftest ,name (check-table ',name))))

(defun split-mismatch (input expected flush)
  "Return how INPUT decodes differently from EXPECTED when split at some
position or fed one byte at a time, or nil when it never does."
  (let ((length (length input)))
    (or (loop for split from 1 below length
              for events = (decode input :splits (list split) :flush flush)
              unless (equalp events expected)
                return (list :split split :events events))
        (let ((events (decode input :splits (loop for i from 1 below length collect i)
                                    :flush flush :copy t)))
          (unless (equalp events expected)
            (list :byte-at-a-time events))))))

(defun check-row (row)
  (let ((input (row-input row))
        (expected (row-events row))
        (flush (row-flush row)))
    (multiple-value-bind (events pending) (decode input :flush flush)
      (is (decodes-to input expected events))
      (is (pending-as-expected input flush pending)))
    (is (same-when-split input (split-mismatch input expected flush)))))

(defun check-table (name)
  (mapc #'check-row (funcall (gethash name *tables*))))

(defun idle-rows ()
  "The rows of every decoder table that leave the decoder idle, so that their
inputs can be concatenated."
  (loop for rows being the hash-values of *tables*
        append (remove-if #'row-flush (funcall rows))))

;;; Esc handling and flushing

(define-decoder-table flushing
  (row-flushed (bytes :esc) (key :escape))
  (row-flushed (bytes :esc :esc) (key :escape :alt))
  (row-flushed (bytes :esc :esc :esc) (key :escape :alt) (key :escape))
  (row-flushed (bytes :esc "[") (key #\[ :alt))
  (row-flushed (bytes :esc "O") (key #\O :alt))
  (row-flushed (bytes :esc "]") (key #\] :alt))
  (row-flushed (bytes :esc "P") (key #\P :alt))
  (row-flushed (bytes :esc "_") (key #\_ :alt))
  (row-flushed (bytes :esc :esc "[") (key :escape) (key #\[ :alt))
  (row-flushed (bytes :esc "[1;") (unknown :esc "[1;"))
  (row-flushed (bytes :esc "[<0;1") (unknown :esc "[<0;1"))
  (row-flushed (bytes :esc "[M") (unknown :esc "[M"))
  (row-flushed (bytes :esc "[M" 32 33) (unknown :esc "[M" 32 33))
  (row-flushed (bytes :esc "]11;rgb:") (unknown :esc "]11;rgb:"))
  (row-flushed (bytes :esc "]11;rgb:" :esc) (unknown :esc "]11;rgb:" :esc))
  (row-flushed (bytes :esc "P1$r") (unknown :esc "P1$r"))
  (row-flushed (bytes #xe2 #x82) (unknown #xe2 #x82))
  (row-flushed (bytes #xf0 #x9f #x98) (unknown #xf0 #x9f #x98))
  (row-flushed (bytes "a" :esc) (key #\a) (key :escape))
  (row-flushed (bytes :esc #xc3) (key :escape) (unknown #xc3)))

(deftest lone-escape-is-pending-until-flushed
  (let ((decoder (make-decoder)))
    (is (null (feed-decoder decoder (bytes :esc))))
    (is (decoder-pending-p decoder))
    (is (equalp (list (key :escape)) (flush-decoder decoder)))
    (is (not (decoder-pending-p decoder)))
    (is (null (flush-decoder decoder)))))

(deftest escape-then-sequence-is-the-sequence
  (let ((decoder (make-decoder)))
    (is (null (feed-decoder decoder (bytes :esc))))
    (is (equalp (list (key :up)) (feed-decoder decoder (bytes "[A"))))
    (is (null (feed-decoder decoder (bytes :esc "["))))
    (is (equalp (list (key :f5 :ctrl)) (feed-decoder decoder (bytes "15;5~"))))))

(deftest escape-then-character-is-alt
  (let ((decoder (make-decoder)))
    (is (null (feed-decoder decoder (bytes :esc))))
    (is (equalp (list (key #\x :alt)) (feed-decoder decoder (bytes "x"))))
    (is (null (feed-decoder decoder (bytes :esc))))
    (is (equalp (list (key (code-char 233) :alt)) (feed-decoder decoder (bytes #xc3 #xa9))))))

(deftest flushed-escape-does-not-combine-with-later-input
  (let ((decoder (make-decoder)))
    (feed-decoder decoder (bytes :esc))
    (is (equalp (list (key :escape)) (flush-decoder decoder)))
    (is (equalp (list (key #\[) (key #\A)) (feed-decoder decoder (bytes "[A"))))))

(deftest flush-leaves-a-paste-alone
  (let ((decoder (make-decoder)))
    (is (null (feed-decoder decoder (bytes :esc "[200~abc"))))
    (is (not (decoder-pending-p decoder)))
    (is (null (flush-decoder decoder)))
    (is (null (feed-decoder decoder (bytes "d" :esc "[20"))))
    (is (not (decoder-pending-p decoder)))
    (is (null (flush-decoder decoder)))
    (is (equalp (list (paste "abcd") (key #\x))
                (feed-decoder decoder (bytes "1~x"))))))

(deftest feed-respects-start-and-end
  (let ((decoder (make-decoder))
        (input (bytes "ab" :esc "[Acd")))
    (is (equalp (list (key #\b) (key :up)) (feed-decoder decoder input :start 1 :end 5)))
    (is (equalp (list (key #\c)) (feed-decoder decoder input :start 5 :end 6)))
    (is (null (feed-decoder decoder input :start 6 :end 6)))))

;;; One representation regardless of encoding

(defun check-equal-encodings (pairs)
  "Check that each of PAIRS, (legacy-bytes kitty-bytes) or (legacy-bytes
kitty-bytes t) when the legacy bytes need a flush, decodes to equal events."
  (loop for (legacy kitty flush) in pairs
        do (is (same-events legacy kitty (decode legacy :flush flush) (decode kitty)))))

(deftest legacy-and-kitty-text-keys-decode-equal
  (check-equal-encodings
   (list (list (bytes "a") (bytes :esc "[97u"))
         (list (bytes "A") (bytes :esc "[97;2u"))
         (list (bytes " ") (bytes :esc "[32u"))
         (list (bytes (string (code-char 233))) (bytes :esc "[233u"))
         (list (bytes 1) (bytes :esc "[97;5u"))
         (list (bytes 0) (bytes :esc "[32;5u"))
         (list (bytes 8) (bytes :esc "[104;5u"))
         (list (bytes 10) (bytes :esc "[106;5u"))
         (list (bytes 28) (bytes :esc "[52;5u"))
         (list (bytes :esc "a") (bytes :esc "[97;3u"))
         (list (bytes :esc "A") (bytes :esc "[97;4u"))
         (list (bytes :esc 1) (bytes :esc "[97;7u"))
         (list (bytes :esc "[27;5;97~") (bytes :esc "[97;5u"))
         (list (bytes :esc "[27;2;65~") (bytes "A")))))

(deftest legacy-and-kitty-named-keys-decode-equal
  (check-equal-encodings
   (list (list (bytes 13) (bytes :esc "[13u"))
         (list (bytes 9) (bytes :esc "[9u"))
         (list (bytes 127) (bytes :esc "[127u"))
         (list (bytes :esc) (bytes :esc "[27u") t)
         (list (bytes :esc :esc) (bytes :esc "[27;3u") t)
         (list (bytes :esc 13) (bytes :esc "[13;3u"))
         (list (bytes :esc 127) (bytes :esc "[127;3u"))
         (list (bytes :esc "[Z") (bytes :esc "[9;2u"))
         (list (bytes :esc :esc "[Z") (bytes :esc "[9;4u"))
         (list (bytes :esc "[A") (bytes :esc "[1;1A"))
         (list (bytes :esc "OA") (bytes :esc "[A"))
         (list (bytes :esc :esc "[A") (bytes :esc "[1;3A"))
         (list (bytes :esc "OP") (bytes :esc "[11~"))
         (list (bytes :esc "[P") (bytes :esc "OP"))
         (list (bytes :esc "OR") (bytes :esc "[13~"))
         (list (bytes :esc "[1;5P") (bytes :esc "[11;5~"))
         (list (bytes :esc "[H") (bytes :esc "[1~"))
         (list (bytes :esc "[7~") (bytes :esc "OH"))
         (list (bytes :esc "[F") (bytes :esc "[4~"))
         (list (bytes :esc "[8~") (bytes :esc "OF"))
         (list (bytes :esc "[E") (bytes :esc "[57427u"))
         (list (bytes :esc "[27;3;13~") (bytes :esc "[13;3u")))))

(deftest kitty-alternates-match-legacy-keys
  (loop for (legacy kitty) in (list (list (bytes "A") (bytes :esc "[97:65;2u"))
                                    (list (bytes "!") (bytes :esc "[49:33;2u"))
                                    (list (bytes 1) (bytes :esc "[97::97;5u"))
                                    (list (bytes "a") (bytes :esc "[97;;97u")))
        do (is (same-keys legacy kitty (decode legacy) (decode kitty)))))

(deftest key-event=-compares-key-modifiers-and-kind
  (is (key-event= (key #\A :shifted #\A :base #\a :text "A") (key #\A)))
  (is (key-event= (key :up :ctrl :keypad) (key :up :ctrl)))
  (is (not (key-event= (key #\a) (key #\A))))
  (is (not (key-event= (key #\a :ctrl) (key #\a))))
  (is (not (key-event= (key #\a :release) (key #\a)))))

;;; Descriptions

(deftest describe-event-is-short-and-readable
  (is (equal "key ctrl+alt+a press" (describe-event (key #\a :ctrl :alt))))
  (is (equal "key shift+:f5 release" (describe-event (key :f5 :shift :release))))
  (is (equal "key space press" (describe-event (key #\Space))))
  (is (equal "key A press shifted=A base=a text=\"A\" keypad"
             (describe-event (key #\A :shifted #\A :base #\a :text "A" :keypad))))
  (is (equal "mouse drag left 10,4 ctrl" (describe-event (mouse :drag :left 10 4 :ctrl))))
  (is (equal "mouse scroll-up 0,0" (describe-event (mouse :scroll-up nil 0 0))))
  (is (equal "paste \"hello\"" (describe-event (paste "hello"))))
  (is (equal "paste \"a^[b^J\"" (describe-event (paste (coerce (list #\a (code-char 27) #\b
                                                                     #\Newline)
                                                               'string)))))
  (is (equal "focus in" (describe-event (make-focus-event :focused t))))
  (is (equal "focus out" (describe-event (make-focus-event :focused nil))))
  (is (equal "resize 80x24" (describe-event (make-resize-event :width 80 :height 24))))
  (is (equal "unknown #(27 91 57 57 120)" (describe-event (unknown :esc "[99x"))))
  (is (equal "kitty-keyboard-reply flags=5"
             (describe-event (make-kitty-keyboard-reply :flags 5))))
  (is (equal "mode-report ?2026 value=2"
             (describe-event (make-mode-report :mode 2026 :value 2 :private t))))
  (is (equal "device-attributes-reply 62;22"
             (describe-event (make-device-attributes-reply :params '(62 22))))))
