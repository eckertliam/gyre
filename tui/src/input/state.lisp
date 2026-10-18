;;;; state.lisp — the input decoder's state, its held bytes and the events it collects.
;;;;
;;;; An ESC that prefixes another key is not held as a byte but recorded in the ALT
;;;; flag, which the next event consumes: a key gains Alt, anything else is
;;;; preceded by an Esc key.

(defpackage #:tui/input/state
  (:use #:cl)
  (:import-from #:tui/input/event
                #:+alt+ #:make-key-event #:key-event-p #:key-event-modifiers #:make-unknown-event)
  (:import-from #:tui/input/params #:make-params #:params)
  (:export #:octets #:+esc+ #:+buffer-size+ #:+paste-buffer-size+
           #:decoder #:make-decoder #:decoder-state #:decoder-alt #:decoder-bytes #:decoder-fill
           #:decoder-utf8-need #:decoder-utf8-low #:decoder-utf8-high #:decoder-utf8-code
           #:decoder-string-esc #:decoder-params
           #:decoder-paste #:decoder-paste-fill #:decoder-paste-match
           #:emit #:take-events #:emit-event #:emit-key #:emit-unknown #:emit-sequence
           #:hold #:reset #:abandon))

(in-package #:tui/input/state)

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defconstant +buffer-size+ 4096
  "Bytes held for one sequence. Longer OSC, DCS and APC strings are reported in pieces.")

(defconstant +paste-buffer-size+ 1024
  "Initial size of the paste buffer, which grows as needed.")

(defconstant +esc+ #x1b)

(deftype state ()
  '(member :ground :utf8 :esc :csi :ss3 :x10 :osc :dcs :apc :paste))

(defstruct (decoder (:constructor %make-decoder) (:copier nil))
  "Terminal input decoder state. Create with MAKE-DECODER."
  (state :ground :type state)
  ;; An ESC prefix waiting to apply Alt to the next event.
  (alt nil :type boolean)
  ;; The bytes of the sequence or character being decoded.
  (bytes (make-array +buffer-size+ :element-type '(unsigned-byte 8)) :type octets)
  (fill 0 :type fixnum)
  ;; UTF-8: continuation bytes still needed, the bounds of the next one, the code so far.
  (utf8-need 0 :type fixnum)
  (utf8-low #x80 :type fixnum)
  (utf8-high #xbf :type fixnum)
  (utf8-code 0 :type (unsigned-byte 21))
  ;; In an OSC, DCS or APC string, the last byte was ESC.
  (string-esc nil :type boolean)
  (params (make-params) :type params)
  ;; Bracketed paste: the bytes so far and how much of ESC [ 2 0 1 ~ they end with.
  (paste (make-array +paste-buffer-size+ :element-type '(unsigned-byte 8)) :type octets)
  (paste-fill 0 :type fixnum)
  (paste-match 0 :type fixnum)
  ;; Events decoded by the current call, oldest first.
  (head nil :type list)
  (tail nil :type list))

(defun make-decoder ()
  "Return a new input decoder. A decoder is a pure state machine: it holds no
clock, thread or file descriptor, and decodes whatever bytes it is given."
  (%make-decoder))

;;; Collecting events

(defun emit (decoder event)
  "Add EVENT to the events of the current call."
  (declare (optimize speed) (type decoder decoder))
  (let ((cell (list event)))
    (if (decoder-tail decoder)
        (setf (cdr (decoder-tail decoder)) cell)
        (setf (decoder-head decoder) cell))
    (setf (decoder-tail decoder) cell)))

(defun take-events (decoder)
  "Return the events of the current call, oldest first, and forget them."
  (declare (type decoder decoder))
  (prog1 (decoder-head decoder)
    (setf (decoder-head decoder) nil
          (decoder-tail decoder) nil)))

(defun emit-event (decoder event)
  "Emit EVENT, consuming a pending ESC prefix: a key without Alt gains Alt, and
anything else is preceded by an Esc key."
  (declare (type decoder decoder))
  (when (decoder-alt decoder)
    (setf (decoder-alt decoder) nil)
    (if (and (key-event-p event) (not (logtest (key-event-modifiers event) +alt+)))
        (setf (key-event-modifiers event) (logior (key-event-modifiers event) +alt+))
        (emit decoder (make-key-event :key :escape))))
  (emit decoder event))

(defun emit-key (decoder key modifiers)
  "Emit a press of KEY with MODIFIERS (see EMIT-EVENT)."
  (declare (type decoder decoder))
  (emit-event decoder (make-key-event :key key :modifiers modifiers)))

(defun emit-unknown (decoder)
  "Emit the held bytes as an unknown event, if there are any."
  (declare (type decoder decoder))
  (let ((fill (decoder-fill decoder)))
    (when (plusp fill)
      (emit-event decoder (make-unknown-event :bytes (subseq (decoder-bytes decoder) 0 fill))))))

;;; Held bytes

(declaim (inline hold))
(defun hold (decoder byte)
  "Add BYTE to the held bytes."
  (declare (optimize speed) (type decoder decoder) (type (unsigned-byte 8) byte))
  (let ((fill (decoder-fill decoder)))
    (setf (aref (decoder-bytes decoder) fill) byte
          (decoder-fill decoder) (1+ fill))))

(defun reset (decoder)
  "Return to the ground state, dropping the held bytes (already emitted)."
  (declare (type decoder decoder))
  (setf (decoder-state decoder) :ground
        (decoder-fill decoder) 0
        (decoder-string-esc decoder) nil))

(defun introducer-only-p (decoder)
  "True when the held bytes are just ESC and one introducer byte."
  (declare (type decoder decoder))
  (and (= 2 (decoder-fill decoder))
       (= +esc+ (aref (decoder-bytes decoder) 0))))

(defun abandon (decoder)
  "Give up on the held sequence: ESC and a lone introducer byte become Alt+that
character, anything longer an unknown event. Return to the ground state."
  (declare (type decoder decoder))
  (if (introducer-only-p decoder)
      (emit-key decoder (code-char (aref (decoder-bytes decoder) 1)) +alt+)
      (emit-unknown decoder))
  (reset decoder))

(defun emit-sequence (decoder event)
  "Emit EVENT for the held sequence, or the held bytes as an unknown event when
EVENT is nil. Return to the ground state."
  (declare (type decoder decoder))
  (if event
      (emit-event decoder event)
      (emit-unknown decoder))
  (reset decoder))
