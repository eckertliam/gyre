;;;; decoder.lisp — the input decoder: a pure state machine from terminal bytes to events.
;;;;
;;;; The decoder consumes one byte at a time, so input may be split anywhere. Bytes
;;;; that don't make an event yet are held in the decoder (see state.lisp).

(defpackage #:tui/input/decoder
  (:use #:cl)
  (:import-from #:tui/input/params #:params-reset #:params-feed #:params-finish)
  (:import-from #:tui/input/keys #:c0-key)
  (:import-from #:tui/input/csi #:csi-event #:ss3-event #:paste-start-p #:x10-start-p)
  (:import-from #:tui/input/mouse #:x10-mouse-event)
  (:import-from #:tui/input/state
                #:octets #:+esc+ #:+buffer-size+
                #:decoder #:make-decoder #:decoder-state #:decoder-alt #:decoder-bytes
                #:decoder-fill #:decoder-utf8-need #:decoder-utf8-low #:decoder-utf8-high
                #:decoder-utf8-code #:decoder-string-esc #:decoder-params #:decoder-paste-match
                #:take-events #:emit-key #:emit-unknown #:emit-sequence #:hold #:reset #:abandon)
  (:import-from #:tui/input/paste #:start-paste #:paste-byte #:feed-paste)
  (:export #:decoder #:make-decoder #:feed-decoder #:flush-decoder #:decoder-pending-p))

(in-package #:tui/input/decoder)

(defconstant +max-csi-length+ 256
  "A CSI sequence that grows longer than this is malformed.")

;;; States. Each handler takes the next byte and returns true when it consumed
;;; it, or false after changing state so that the byte is decoded again.

(defun utf8-lead (byte)
  "Return how many continuation bytes follow the UTF-8 lead byte BYTE and the
bounds of the first one; 0 when BYTE can't begin a character."
  (declare (type (unsigned-byte 8) byte))
  (cond ((<= #xc2 byte #xdf) (values 1 #x80 #xbf))
        ((= byte #xe0) (values 2 #xa0 #xbf))
        ((= byte #xed) (values 2 #x80 #x9f))
        ((<= #xe1 byte #xef) (values 2 #x80 #xbf))
        ((= byte #xf0) (values 3 #x90 #xbf))
        ((<= #xf1 byte #xf3) (values 3 #x80 #xbf))
        ((= byte #xf4) (values 3 #x80 #x8f))
        (t (values 0 0 0))))

(defun start-utf8 (decoder byte)
  (declare (type decoder decoder) (type (unsigned-byte 8) byte))
  (hold decoder byte)
  (multiple-value-bind (need low high) (utf8-lead byte)
    (declare (type fixnum need low high))
    (if (zerop need)
        (progn (emit-unknown decoder) (reset decoder))
        (setf (decoder-state decoder) :utf8
              (decoder-utf8-need decoder) need
              (decoder-utf8-low decoder) low
              (decoder-utf8-high decoder) high
              (decoder-utf8-code decoder) (logand byte (ash #x3f (- need)))))))

(defun ground-byte (decoder byte)
  (declare (optimize speed) (type decoder decoder) (type (unsigned-byte 8) byte))
  (cond ((= byte +esc+)
         (hold decoder byte)
         (setf (decoder-state decoder) :esc))
        ((or (< byte #x20) (= byte #x7f))
         (multiple-value-bind (key modifiers) (c0-key byte)
           (emit-key decoder key modifiers)))
        ((< byte #x80) (emit-key decoder (code-char byte) 0))
        (t (start-utf8 decoder byte)))
  t)

(defun utf8-byte (decoder byte)
  (declare (optimize speed) (type decoder decoder) (type (unsigned-byte 8) byte))
  (cond ((<= (decoder-utf8-low decoder) byte (decoder-utf8-high decoder))
         (hold decoder byte)
         (setf (decoder-utf8-code decoder) (logior (ash (decoder-utf8-code decoder) 6)
                                                   (logand byte #x3f))
               (decoder-utf8-low decoder) #x80
               (decoder-utf8-high decoder) #xbf)
         (when (zerop (decf (decoder-utf8-need decoder)))
           (emit-key decoder (code-char (decoder-utf8-code decoder)) 0)
           (reset decoder))
         t)
        (t (abandon decoder) nil)))

(defun esc-byte (decoder byte)
  (declare (type decoder decoder) (type (unsigned-byte 8) byte))
  (flet ((enter (state)
           (hold decoder byte)
           (setf (decoder-state decoder) state)))
    (case (code-char byte)
      (#\[ (params-reset (decoder-params decoder)) (enter :csi))
      (#\O (enter :ss3))
      (#\] (enter :osc))
      (#\P (enter :dcs))
      (#\_ (enter :apc))
      (#\Esc
       ;; ESC ESC: the first is an Alt prefix. A third makes the first two Alt+Esc.
       (if (decoder-alt decoder)
           (emit-key decoder :escape 0)
           (setf (decoder-alt decoder) t)))
      (t (setf (decoder-alt decoder) t)
         (reset decoder)
         (return-from esc-byte nil))))
  t)

(defun finish-csi (decoder final)
  (declare (type decoder decoder) (type (unsigned-byte 8) final))
  (let ((params (decoder-params decoder)))
    (params-finish params)
    (cond ((paste-start-p params final) (start-paste decoder))
          ((x10-start-p params final) (setf (decoder-state decoder) :x10))
          (t (emit-sequence decoder (csi-event params final))))))

(defun csi-byte (decoder byte)
  (declare (optimize speed) (type decoder decoder) (type (unsigned-byte 8) byte))
  (cond ((<= #x40 byte #x7e)
         (hold decoder byte)
         (finish-csi decoder byte)
         t)
        ((and (<= #x20 byte #x3f)
              (< (decoder-fill decoder) +max-csi-length+)
              (params-feed (decoder-params decoder) byte))
         (hold decoder byte)
         t)
        (t (abandon decoder) nil)))

(defun ss3-byte (decoder byte)
  (declare (type decoder decoder) (type (unsigned-byte 8) byte))
  (cond ((<= #x40 byte #x7e)
         (hold decoder byte)
         (emit-sequence decoder (ss3-event byte))
         t)
        (t (abandon decoder) nil)))

(defun x10-byte (decoder byte)
  "The three raw bytes after CSI M: button, column and row."
  (declare (type decoder decoder) (type (unsigned-byte 8) byte))
  (hold decoder byte)
  (when (= 6 (decoder-fill decoder))
    (let ((bytes (decoder-bytes decoder)))
      (emit-sequence decoder (x10-mouse-event (aref bytes 3) (aref bytes 4) (aref bytes 5)))))
  t)

;;; OSC, DCS and APC strings, which end with ST (ESC \), or BEL for OSC

(defun string-hold (decoder byte)
  "Hold BYTE of a string, first reporting the held bytes as an unknown event if
the buffer has no room left for it and a final byte."
  (declare (type decoder decoder) (type (unsigned-byte 8) byte))
  (when (>= (decoder-fill decoder) (1- +buffer-size+))
    (emit-unknown decoder)
    (setf (decoder-fill decoder) 0))
  (hold decoder byte))

(defun string-escape-byte (decoder byte)
  "The byte after an ESC inside a string."
  (declare (type decoder decoder) (type (unsigned-byte 8) byte))
  (cond ((= byte #x5c)
         (hold decoder byte)
         (emit-unknown decoder)
         (reset decoder)
         t)
        (t
         ;; Any other byte means the string was cut short and the ESC begins
         ;; a new sequence.
         (decf (decoder-fill decoder))
         (abandon decoder)
         (hold decoder +esc+)
         (setf (decoder-state decoder) :esc)
         nil)))

(defun string-byte (decoder byte)
  (declare (type decoder decoder) (type (unsigned-byte 8) byte))
  (cond ((decoder-string-esc decoder) (string-escape-byte decoder byte))
        ((and (= byte 7) (eq (decoder-state decoder) :osc))
         (hold decoder byte)
         (emit-unknown decoder)
         (reset decoder)
         t)
        ((= byte +esc+)
         (string-hold decoder byte)
         (setf (decoder-string-esc decoder) t)
         t)
        ((or (< byte #x20) (= byte #x7f))
         ;; Replies never contain other control characters: this was a key.
         (abandon decoder)
         nil)
        (t (string-hold decoder byte) t)))

;;; Entry points

(defun step-byte (decoder byte)
  (declare (optimize speed) (type decoder decoder) (type (unsigned-byte 8) byte))
  (loop until (ecase (decoder-state decoder)
                (:ground (ground-byte decoder byte))
                (:utf8 (utf8-byte decoder byte))
                (:esc (esc-byte decoder byte))
                (:csi (csi-byte decoder byte))
                (:ss3 (ss3-byte decoder byte))
                (:x10 (x10-byte decoder byte))
                ((:osc :dcs :apc) (string-byte decoder byte))
                (:paste (paste-byte decoder byte)))))

(defun feed-decoder (decoder octets &key (start 0) end)
  "Decode the bytes of OCTETS from START to END (default: its length) and return
a fresh list of the events they complete, in order. Input may be split at any
byte: what doesn't make an event yet stays in DECODER for the next call (see
DECODER-PENDING-P). Nothing is ever dropped: bytes the decoder doesn't
understand come back as UNKNOWN-EVENTs."
  (declare (optimize speed) (type decoder decoder) (type octets octets))
  (let ((end (or end (length octets)))
        (i start))
    (declare (type fixnum end i))
    (assert (<= 0 start end (length octets)))
    ;; Drop anything left behind by a call that was unwound.
    (take-events decoder)
    (loop while (< i end)
          do (if (and (eq (decoder-state decoder) :paste)
                      (= 0 (decoder-paste-match decoder))
                      (/= +esc+ (aref octets i)))
                 (setf i (feed-paste decoder octets i end))
                 (progn (step-byte decoder (aref octets i))
                        (incf i))))
    (take-events decoder)))

(defun decoder-pending-p (decoder)
  "True when DECODER holds bytes that haven't made an event yet: a lone ESC, the
start of an escape sequence or part of a UTF-8 character. The caller should call
FLUSH-DECODER if no more input arrives within its Esc timeout. False inside a
bracketed paste, which is never cut short."
  (declare (type decoder decoder))
  (case (decoder-state decoder)
    ((:ground :paste) nil)
    (t t)))

(defun flush-decoder (decoder)
  "Resolve the bytes DECODER holds as if no more input will follow them, and
return the resulting events. A lone ESC becomes the Esc key, ESC ESC Alt+Esc,
and ESC followed by just [, O, ], P or _ Alt+that character. Anything else
incomplete becomes an UNKNOWN-EVENT with its bytes. A bracketed paste in
progress is left alone."
  (declare (type decoder decoder))
  (ecase (decoder-state decoder)
    ((:ground :paste))
    (:esc (reset decoder)
     (emit-key decoder :escape 0))
    ((:utf8 :csi :ss3 :x10 :osc :dcs :apc) (abandon decoder)))
  (take-events decoder))
