;;;; unicode.lisp — the Unicode properties tui/width needs, cached per code point.
;;;;
;;;; Looking a property up in SB-UNICODE's tables costs tens of nanoseconds, which adds up
;;;; when every cell of a frame holds non-ASCII text. The properties of a character never
;;;; change, so they are computed once and kept in a table of 256-entry blocks, allocated on
;;;; first use. The table is a pure cache of SB-UNICODE's answers, shared by everything in
;;;; the process.

(defpackage #:tui/unicode
  (:use #:cl)
  (:export #:char-width
           #:break-class
           #:extended-pictographic-p
           #:emoji-p))

(in-package #:tui/unicode)

;;; Computing properties

(sb-ext:defglobal **break-classes**
  #(nil :cr :lf :control :extend :zwj :regional-indicator :prepend :spacing-mark
    :l :v :t :lv :lvt)
  "The values of SB-UNICODE:GRAPHEME-BREAK-CLASS, indexed by their code in a property set.")

(declaim (type simple-vector **break-classes**))

(defun control-code-p (code)
  (or (< code #x20) (<= #x7F code #x9F)))

(defun wide-char-p (char)
  (or (member (sb-unicode:east-asian-width char) '(:w :f))
      (sb-unicode:proplist-p char :emoji-presentation)))

(defun zero-width-char-p (char)
  (let ((code (char-code char)))
    (or (member (sb-unicode:general-category char) '(:mn :me :cf))
        ;; Hangul Jungseong and Jongseong Jamo combine with a preceding Choseong.
        (<= #x1160 code #x11FF)
        (<= #xD7B0 code #xD7FF))))

(defun compute-width (char)
  (let ((code (char-code char)))
    (cond ((control-code-p code) 0)
          ((< code #x7F) 1)
          ((= code #xAD) 1)              ; soft hyphen, drawn by terminals
          ((zero-width-char-p char) 0)
          ((wide-char-p char) 2)
          (t 1))))

(defconstant +computed+ #x100 "Set in every computed property set.")

(defun compute-properties (char)
  "CHAR's properties packed in a fixnum: width in bits 0–1, break class code in bits 2–5,
Extended_Pictographic in bit 6, Emoji in bit 7, and +COMPUTED+."
  (logior +computed+
          (compute-width char)
          (ash (position (sb-unicode:grapheme-break-class char) **break-classes**) 2)
          (if (sb-unicode:proplist-p char :extended-pictographic) #x40 0)
          (if (sb-unicode:proplist-p char :emoji) #x80 0)))

;;; The cache

(deftype property-block () '(simple-array (unsigned-byte 16) (256)))

(sb-ext:defglobal **blocks** (make-array (ash char-code-limit -8) :initial-element nil)
  "Property blocks indexed by code point / 256; NIL until a character of the block is seen.")

(declaim (type simple-vector **blocks**))

(defun fill-properties (char)
  (let* ((code (char-code char))
         (block (or (svref **blocks** (ash code -8))
                    (setf (svref **blocks** (ash code -8))
                          (make-array 256 :element-type '(unsigned-byte 16)
                                          :initial-element 0)))))
    (declare (type property-block block))
    (setf (aref block (logand code 255)) (compute-properties char))))

(declaim (inline properties))
(defun properties (char)
  (let* ((code (char-code char))
         (block (svref **blocks** (ash code -8))))
    (if block
        (let ((properties (aref (the property-block block) (logand code 255))))
          (if (zerop properties) (fill-properties char) properties))
        (fill-properties char))))

;;; Accessors

(declaim (inline char-width break-class extended-pictographic-p emoji-p))

(defun char-width (char)
  "Return how many columns CHAR occupies on its own: 0 for control characters, combining
marks and other zero-width characters, 2 for East Asian wide and full-width characters and
emoji with default emoji presentation, and 1 otherwise."
  (logand (properties char) 3))

(defun break-class (char)
  "CHAR's grapheme break class, as SB-UNICODE:GRAPHEME-BREAK-CLASS returns it."
  (svref **break-classes** (ldb (byte 4 2) (properties char))))

(defun extended-pictographic-p (char)
  "True when CHAR has the Extended_Pictographic property."
  (logtest (properties char) #x40))

(defun emoji-p (char)
  "True when CHAR has the Emoji property."
  (logtest (properties char) #x80))
