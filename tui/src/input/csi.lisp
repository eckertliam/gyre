;;;; csi.lisp — turning complete CSI and SS3 sequences into events.

(defpackage #:tui/input/csi
  (:use #:cl)
  (:import-from #:tui/input/event
                #:+shift+ #:make-key-event #:make-focus-event #:make-kitty-keyboard-reply
                #:make-mode-report #:make-device-attributes-reply)
  (:import-from #:tui/input/params
                #:param #:param-count #:param-field-count #:params-marker #:params-intermediate
                #:params-odd #:params-empty-p)
  (:import-from #:tui/input/keys #:letter-key #:tilde-key #:code-key)
  (:import-from #:tui/input/mouse #:sgr-mouse-event #:urxvt-mouse-event)
  (:export #:csi-event #:ss3-event #:paste-start-p #:x10-start-p))

(in-package #:tui/input/csi)

;;; Modifiers and event kinds

(defun modifier-bits (value)
  "The modifier mask for the wire VALUE of a modifier parameter (mask + 1)."
  (declare (type fixnum value))
  (logand (max 0 (1- value)) #xff))

(defun event-kind (value)
  (declare (type fixnum value))
  (case value
    (2 :repeat)
    (3 :release)
    (t :press)))

(defun fold-shift (key modifiers shifted)
  "Fold shift into the character KEY when the shifted character is known: the
kitty SHIFTED alternate, or the upper case of an ASCII letter. Return the key
and the remaining modifiers."
  (declare (type fixnum modifiers))
  (if (and (characterp key) (logtest modifiers +shift+))
      (let ((unshifted (logandc2 modifiers +shift+)))
        (cond ((characterp shifted) (values shifted unshifted))
              ((char<= #\a key #\z) (values (char-upcase key) unshifted))
              ((char<= #\A key #\Z) (values key unshifted))
              (t (values key modifiers))))
      (values key modifiers)))

(defun modified-key-event (key keypad params &optional (extra-modifiers 0))
  "Return a key event for the named KEY with the modifiers and event kind in
field 1 of PARAMS, as in CSI 1 ; mods:kind A, plus EXTRA-MODIFIERS."
  (declare (type fixnum extra-modifiers))
  (make-key-event :key key
                  :modifiers (logior extra-modifiers (modifier-bits (param params 1 0 1)))
                  :kind (event-kind (param params 1 1 1))
                  :keypad keypad))

;;; Legacy keys

(defun letter-key-event (params final)
  "CSI A, CSI 1 ; mods A and the like."
  (multiple-value-bind (key keypad) (letter-key final)
    (when (and key (= 1 (param params 0 0 1)))
      (modified-key-event key keypad params))))

(defun backtab-event (params)
  "CSI Z: shift+tab."
  (when (= 1 (param params 0 0 1))
    (modified-key-event :tab nil params +shift+)))

(defun other-keys-event (params)
  "xterm modifyOtherKeys: CSI 27 ; mods ; key ~."
  (let ((code (param params 2)))
    (when (>= code 0)
      (multiple-value-bind (key keypad) (code-key code)
        (when key
          (multiple-value-bind (key modifiers)
              (fold-shift key (modifier-bits (param params 1 0 1)) nil)
            (make-key-event :key key :modifiers modifiers :keypad keypad
                            :kind (event-kind (param params 1 1 1)))))))))

(defun tilde-event (params)
  "CSI n ~ and CSI n ; mods ~ keys."
  (let ((n (param params 0)))
    (if (= n 27)
        (other-keys-event params)
        (multiple-value-bind (key keypad)
            ;; Kitty documents KP_BEGIN as CSI 57427 ~.
            (if (>= n 57344) (code-key n) (tilde-key n))
          (when (keywordp key)
            (modified-key-event key keypad params))))))

;;; Kitty keys: CSI code:shifted:base ; mods:kind ; text u

(defun codepoint-char (code)
  (declare (type fixnum code))
  (if (or (>= code char-code-limit) (<= #xd800 code #xdfff))
      (code-char #xfffd)
      (code-char code)))

(defun kitty-text (params)
  "The text field (field 2) of a kitty key sequence as a string, or nil."
  (let ((count (param-count params 2))
        (fill 0))
    (declare (type fixnum count fill))
    (when (plusp count)
      (let ((text (make-string count)))
        (dotimes (i count)
          (let ((code (param params 2 i)))
            (when (>= code 0)
              (setf (char text fill) (codepoint-char code))
              (incf fill))))
        (cond ((= fill count) text)
              ((plusp fill) (subseq text 0 fill)))))))

(defun alternate-key (params sub)
  "The kitty alternate key in value SUB of field 0, or nil."
  (let ((code (param params 0 sub)))
    (when (>= code 0)
      (values (code-key code)))))

(defun kitty-key-event (params)
  "CSI code:shifted:base ; mods:kind ; text u."
  (let* ((code (param params 0))
         (text (kitty-text params)))
    (multiple-value-bind (key keypad)
        ;; Key number 0 means the text matches no key.
        (if (= code 0)
            (and text (char text 0))
            (code-key code))
      (when key
        (let ((shifted (alternate-key params 1)))
          (multiple-value-bind (key modifiers)
              (fold-shift key (modifier-bits (param params 1 0 1)) shifted)
            (make-key-event :key key
                            :modifiers modifiers
                            :kind (event-kind (param params 1 1 1))
                            :shifted-key shifted
                            :base-layout-key (alternate-key params 2)
                            :text text
                            :keypad keypad)))))))

;;; Replies to feature queries

(defun mode-report-event (params private)
  "DECRPM: CSI ? mode ; value $ y, or CSI mode ; value $ y when not PRIVATE."
  (let ((mode (param params 0))
        (value (param params 1)))
    (when (and (>= mode 0) (>= value 0))
      (make-mode-report :mode mode :value value :private private))))

(defun field-values (params)
  "The first value of every field of PARAMS, empty ones as 0."
  (unless (params-empty-p params)
    (loop for field below (param-field-count params)
          collect (param params field 0 0))))

(defun private-csi-event (params final)
  "CSI ? ... u (kitty keyboard flags) and CSI ? ... c (device attributes)."
  (case final
    (#\u (make-kitty-keyboard-reply :flags (param params 0 0 0)))
    (#\c (make-device-attributes-reply :params (field-values params)))))

;;; Dispatch

(defun plain-csi-event (params final)
  "CSI sequences without a private marker or intermediate byte."
  (case final
    ((#\A #\B #\C #\D #\E #\F #\H #\P #\Q #\R #\S) (letter-key-event params final))
    (#\Z (backtab-event params))
    (#\~ (tilde-event params))
    (#\u (kitty-key-event params))
    (#\I (when (params-empty-p params) (make-focus-event :focused t)))
    (#\O (when (params-empty-p params) (make-focus-event :focused nil)))
    (#\M (urxvt-mouse-event params))))

(defun csi-event (params final)
  "Return the event for the complete CSI sequence with the finished PARAMS and
final byte FINAL, or nil when it is not a sequence this decoder recognizes."
  (declare (type (unsigned-byte 8) final))
  (let ((final (code-char final))
        (marker (code-char (params-marker params)))
        (intermediate (code-char (params-intermediate params))))
    (cond ((params-odd params) nil)
          ((char= intermediate #\$)
           (when (and (char= final #\y) (member marker '(#\Nul #\?)))
             (mode-report-event params (char= marker #\?))))
          ((char/= intermediate #\Nul) nil)
          ((char= marker #\Nul) (plain-csi-event params final))
          ((char= marker #\?) (private-csi-event params final))
          ((and (char= marker #\<) (member final '(#\M #\m)))
           (sgr-mouse-event params (char= final #\m))))))

(defun ss3-event (final)
  "Return the key event for SS3 followed by the byte FINAL, or nil."
  (declare (type (unsigned-byte 8) final))
  (multiple-value-bind (key keypad) (letter-key (code-char final))
    (when key
      (make-key-event :key key :keypad keypad))))

(defun paste-start-p (params final)
  "True when PARAMS and FINAL make CSI 200 ~, the start of a bracketed paste."
  (declare (type (unsigned-byte 8) final))
  (and (= final #x7e)
       (= 0 (params-marker params) (params-intermediate params))
       (not (params-odd params))
       (= 1 (param-field-count params) (param-count params 0))
       (= 200 (param params 0))))

(defun x10-start-p (params final)
  "True when PARAMS and FINAL make a bare CSI M, which three raw bytes of an X10
mouse report follow."
  (declare (type (unsigned-byte 8) final))
  (and (= final #x4d)
       (params-empty-p params)
       (= 0 (params-marker params) (params-intermediate params))))
