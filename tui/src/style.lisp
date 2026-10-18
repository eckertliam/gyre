;;;; style.lisp — colors, text modifiers and styles.
;;;;
;;;; Colors and modifier sets are fixnums, so a buffer can store them in specialized arrays
;;;; without a heap object per cell. A style is a small immutable struct: three colors and
;;;; two modifier sets don't fit in one fixnum.

(defpackage #:tui/style
  (:use #:cl)
  (:export #:color
           #:color-designator
           #:invalid-color
           #:rgb
           #:indexed
           #:rgb-color-p
           #:color-red
           #:color-green
           #:color-blue
           #:+reset+
           #:+black+
           #:+red+
           #:+green+
           #:+yellow+
           #:+blue+
           #:+magenta+
           #:+cyan+
           #:+gray+
           #:+dark-gray+
           #:+light-red+
           #:+light-green+
           #:+light-yellow+
           #:+light-blue+
           #:+light-magenta+
           #:+light-cyan+
           #:+white+
           #:modifiers
           #:modifier-set
           #:invalid-modifier
           #:+bold+
           #:+dim+
           #:+italic+
           #:+underline+
           #:+double-underline+
           #:+curly-underline+
           #:+dotted-underline+
           #:+dashed-underline+
           #:+any-underline+
           #:+blink+
           #:+reverse+
           #:+hidden+
           #:+strikethrough+
           #:underline-style
           #:style
           #:stylep
           #:make-style
           #:style-fg
           #:style-bg
           #:style-underline-color
           #:style-add-modifiers
           #:style-sub-modifiers
           #:patch-style
           #:apply-modifiers
           #:style=))

(in-package #:tui/style)

;;; Colors

(deftype color ()
  "A terminal color packed into a fixnum. 0–255 is an indexed palette color, of which 0–15
are the 16 ANSI colors. +RESET+ is the terminal's default color. An RGB color has bit 24
set and its red, green and blue components in bits 16–23, 8–15 and 0–7."
  '(integer 0 #x1FFFFFF))

(defconstant +reset+ 256 "The terminal's default foreground or background color.")
(defconstant +black+ 0)
(defconstant +red+ 1)
(defconstant +green+ 2)
(defconstant +yellow+ 3)
(defconstant +blue+ 4)
(defconstant +magenta+ 5)
(defconstant +cyan+ 6)
(defconstant +gray+ 7)
(defconstant +dark-gray+ 8)
(defconstant +light-red+ 9)
(defconstant +light-green+ 10)
(defconstant +light-yellow+ 11)
(defconstant +light-blue+ 12)
(defconstant +light-magenta+ 13)
(defconstant +light-cyan+ 14)
(defconstant +white+ 15)

(define-condition invalid-color (error)
  ((designator :initarg :designator :reader invalid-color-designator))
  (:report (lambda (condition stream)
             (format stream "~S does not designate a color."
                     (invalid-color-designator condition))))
  (:documentation "Signaled when a value that should designate a color does not."))

(declaim (inline rgb indexed rgb-color-p color-red color-green color-blue))

(defun rgb (red green blue)
  "Return the 24-bit color with components RED, GREEN and BLUE, each 0–255."
  (declare (type (unsigned-byte 8) red green blue))
  (logior #x1000000 (ash red 16) (ash green 8) blue))

(defun indexed (index)
  "Return the palette color INDEX, 0–255."
  (declare (type (unsigned-byte 8) index))
  index)

(defun rgb-color-p (color)
  "True when COLOR is a 24-bit color."
  (declare (type color color))
  (logbitp 24 color))

(defun color-red (color)
  "The red component of the 24-bit COLOR."
  (declare (type color color))
  (ldb (byte 8 16) color))

(defun color-green (color)
  "The green component of the 24-bit COLOR."
  (declare (type color color))
  (ldb (byte 8 8) color))

(defun color-blue (color)
  "The blue component of the 24-bit COLOR."
  (declare (type color color))
  (ldb (byte 8 0) color))

(defparameter *color-names*
  '((:reset . 256) (:black . 0) (:red . 1) (:green . 2) (:yellow . 3) (:blue . 4)
    (:magenta . 5) (:cyan . 6) (:gray . 7) (:dark-gray . 8) (:light-red . 9)
    (:light-green . 10) (:light-yellow . 11) (:light-blue . 12) (:light-magenta . 13)
    (:light-cyan . 14) (:white . 15))
  "Keyword names of the default color and the 16 ANSI colors.")

(defun parse-hex-color (string)
  (unless (and (= (length string) 7) (char= (char string 0) #\#)
               (every (lambda (c) (digit-char-p c 16)) (subseq string 1)))
    (error 'invalid-color :designator string))
  (let ((value (parse-integer string :start 1 :radix 16)))
    (rgb (ldb (byte 8 16) value) (ldb (byte 8 8) value) (ldb (byte 8 0) value))))

(defun color-designator (designator)
  "Return the color designated by DESIGNATOR: a color (fixnum), a keyword naming the
default color or one of the 16 ANSI colors (:reset, :black, :red, … :light-cyan, :white),
or a string \"#rrggbb\". Signal INVALID-COLOR otherwise."
  (etypecase designator
    (color designator)
    (keyword (or (cdr (assoc designator *color-names*))
                 (error 'invalid-color :designator designator)))
    (string (parse-hex-color designator))
    (t (error 'invalid-color :designator designator))))

;;; Modifiers

(deftype modifier-set ()
  "A set of text modifiers as a bitmask of the +BOLD+ … +STRIKETHROUGH+ constants."
  '(unsigned-byte 12))

(defconstant +bold+ 1)
(defconstant +dim+ 2)
(defconstant +italic+ 4)
(defconstant +underline+ 8)
(defconstant +double-underline+ 16)
(defconstant +curly-underline+ 32)
(defconstant +dotted-underline+ 64)
(defconstant +dashed-underline+ 128)
(defconstant +blink+ 256)
(defconstant +reverse+ 512)
(defconstant +hidden+ 1024)
(defconstant +strikethrough+ 2048)
(defconstant +any-underline+ (logior 8 16 32 64 128)
  "Every underline style; remove this set to remove any underline.")

(define-condition invalid-modifier (error)
  ((designator :initarg :designator :reader invalid-modifier-designator))
  (:report (lambda (condition stream)
             (format stream "~S does not designate a text modifier."
                     (invalid-modifier-designator condition))))
  (:documentation "Signaled when a value that should designate a modifier does not."))

(defparameter *modifier-names*
  '((:bold . 1) (:dim . 2) (:italic . 4) (:underline . 8) (:double-underline . 16)
    (:curly-underline . 32) (:dotted-underline . 64) (:dashed-underline . 128)
    (:blink . 256) (:reverse . 512) (:hidden . 1024) (:strikethrough . 2048))
  "Keyword names of the text modifiers.")

(defun modifiers (designator)
  "Return the modifier set designated by DESIGNATOR: a modifier set (fixnum), a keyword
such as :bold, :italic, :curly-underline or :strikethrough, or a list of those. Signal
INVALID-MODIFIER otherwise."
  (etypecase designator
    (modifier-set designator)
    (keyword (or (cdr (assoc designator *modifier-names*))
                 (error 'invalid-modifier :designator designator)))
    (list (reduce #'logior designator :key #'modifiers :initial-value 0))
    (t (error 'invalid-modifier :designator designator))))

(defun underline-style (modifiers)
  "Return the underline style that MODIFIERS shows: :none, :single, :double, :curly,
:dotted or :dashed. When several underline modifiers are set, the first of curly, double,
dotted, dashed and single wins."
  (declare (type modifier-set modifiers))
  (cond ((logtest modifiers +curly-underline+) :curly)
        ((logtest modifiers +double-underline+) :double)
        ((logtest modifiers +dotted-underline+) :dotted)
        ((logtest modifiers +dashed-underline+) :dashed)
        ((logtest modifiers +underline+) :single)
        (t :none)))

;;; Styles

(defstruct (style (:constructor %make-style
                      (fg bg underline-color add-modifiers sub-modifiers))
                  (:predicate stylep)
                  (:copier nil))
  "How to draw text: colors to set and modifiers to add or remove. A NIL color leaves
the color underneath unchanged, like an unset field in ratatui's Style."
  (fg nil :type (or null color) :read-only t)
  (bg nil :type (or null color) :read-only t)
  (underline-color nil :type (or null color) :read-only t)
  (add-modifiers 0 :type modifier-set :read-only t)
  (sub-modifiers 0 :type modifier-set :read-only t))

(defun make-style (&key fg bg underline-color modifiers remove-modifiers)
  "Return a style. FG, BG and UNDERLINE-COLOR are color designators (see
COLOR-DESIGNATOR) or NIL to leave that color unchanged. MODIFIERS and REMOVE-MODIFIERS
are modifier designators (see MODIFIERS) to add and to remove."
  (flet ((color-or-nil (designator)
           (and designator (color-designator designator))))
    (let ((add (modifiers (or modifiers 0)))
          (sub (modifiers (or remove-modifiers 0))))
      (%make-style (color-or-nil fg) (color-or-nil bg) (color-or-nil underline-color)
                   add (logandc2 sub add)))))

(defun patch-style (base patch)
  "Return BASE with PATCH applied on top, like ratatui's Style::patch: colors set in PATCH
replace those in BASE, and PATCH's added and removed modifiers override BASE's. Either
may be NIL, meaning a style that changes nothing."
  (cond ((null patch) base)
        ((null base) patch)
        (t (let ((add (style-add-modifiers patch))
                 (sub (style-sub-modifiers patch)))
             (%make-style (or (style-fg patch) (style-fg base))
                          (or (style-bg patch) (style-bg base))
                          (or (style-underline-color patch) (style-underline-color base))
                          (logior (logandc2 (style-add-modifiers base) sub) add)
                          (logior (logandc2 (style-sub-modifiers base) add) sub))))))

(declaim (inline apply-modifiers))
(defun apply-modifiers (modifiers style)
  "Return the modifier set MODIFIERS after STYLE (or NIL) adds and removes its modifiers."
  (declare (type modifier-set modifiers))
  (if style
      (logior (logandc2 modifiers (style-sub-modifiers style)) (style-add-modifiers style))
      modifiers))

(defun style= (a b)
  "True when styles A and B (either may be NIL) change the same things."
  (flet ((field (style reader default)
           (if style (funcall reader style) default)))
    (and (eql (field a #'style-fg nil) (field b #'style-fg nil))
         (eql (field a #'style-bg nil) (field b #'style-bg nil))
         (eql (field a #'style-underline-color nil) (field b #'style-underline-color nil))
         (eql (field a #'style-add-modifiers 0) (field b #'style-add-modifiers 0))
         (eql (field a #'style-sub-modifiers 0) (field b #'style-sub-modifiers 0)))))
