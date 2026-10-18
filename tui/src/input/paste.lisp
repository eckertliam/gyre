;;;; paste.lisp — bracketed paste: everything up to ESC [ 2 0 1 ~ is pasted text.

(defpackage #:tui/input/paste
  (:use #:cl)
  (:import-from #:tui/input/event #:make-key-event #:make-paste-event)
  (:import-from #:tui/input/state
                #:octets #:+esc+ #:+paste-buffer-size+ #:decoder #:decoder-state #:decoder-alt
                #:decoder-paste #:decoder-paste-fill #:decoder-paste-match #:emit #:reset)
  (:export #:start-paste #:paste-byte #:feed-paste))

(in-package #:tui/input/paste)

(defun start-paste (decoder)
  "Begin a bracketed paste, its start sequence just decoded."
  (declare (type decoder decoder))
  (when (decoder-alt decoder)
    (setf (decoder-alt decoder) nil)
    (emit decoder (make-key-event :key :escape)))
  (reset decoder)
  (setf (decoder-state decoder) :paste
        (decoder-paste-fill decoder) 0
        (decoder-paste-match decoder) 0))

(defun paste-room (decoder count)
  "Return the paste buffer, grown if needed to take COUNT more bytes."
  (declare (type decoder decoder) (type fixnum count))
  (let ((paste (decoder-paste decoder))
        (needed (+ (decoder-paste-fill decoder) count)))
    (if (<= needed (length paste))
        paste
        (let ((bigger (make-array (max needed (* 2 (length paste)))
                                  :element-type '(unsigned-byte 8))))
          (replace bigger paste :end2 (decoder-paste-fill decoder))
          (setf (decoder-paste decoder) bigger)))))

(defun finish-paste (decoder)
  "Emit the paste, whose bytes end with ESC [ 2 0 1 ~."
  (declare (type decoder decoder))
  (let ((text (sb-ext:octets-to-string
               (decoder-paste decoder)
               :end (- (decoder-paste-fill decoder) 6)
               :external-format '(:utf-8 :replacement #\Replacement_Character))))
    (emit decoder (make-paste-event :text text)))
  (when (> (length (decoder-paste decoder)) (* 64 +paste-buffer-size+))
    ;; Don't keep a huge buffer around after a huge paste.
    (setf (decoder-paste decoder)
          (make-array +paste-buffer-size+ :element-type '(unsigned-byte 8))))
  (setf (decoder-paste-fill decoder) 0
        (decoder-paste-match decoder) 0)
  (reset decoder))

(defun paste-byte (decoder byte)
  "Add BYTE to the paste, emitting the paste when BYTE ends it."
  (declare (optimize speed) (type decoder decoder) (type (unsigned-byte 8) byte))
  (let ((paste (paste-room decoder 1))
        (fill (decoder-paste-fill decoder))
        (match (decoder-paste-match decoder)))
    (declare (type fixnum fill match))
    (setf (aref paste fill) byte
          (decoder-paste-fill decoder) (1+ fill))
    ;; ESC occurs only at the start of the terminator, so a mismatch restarts
    ;; the match at that ESC or at nothing.
    (setf match (cond ((= byte (the fixnum (svref #(27 91 50 48 49 126) match))) (1+ match))
                      ((= byte +esc+) 1)
                      (t 0)))
    (if (= match 6)
        (finish-paste decoder)
        (setf (decoder-paste-match decoder) match)))
  t)

(defun feed-paste (decoder octets start end)
  "Copy paste bytes from OCTETS in bulk, from START up to the next ESC or END.
Return the index of the first byte not copied."
  (declare (optimize speed) (type decoder decoder) (type octets octets) (type fixnum start end))
  (let* ((stop (or (position +esc+ octets :start start :end end) end))
         (count (- stop start))
         (paste (paste-room decoder count)))
    (replace paste octets :start1 (decoder-paste-fill decoder) :start2 start :end2 stop)
    (incf (decoder-paste-fill decoder) count)
    stop))
