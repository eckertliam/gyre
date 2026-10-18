;;;; describe.lisp — short human-readable descriptions of input events.

(defpackage #:tui/input/describe
  (:use #:cl)
  (:import-from #:tui/input/event
                #:+shift+ #:+alt+ #:+ctrl+ #:+super+ #:+hyper+ #:+meta+ #:+caps-lock+ #:+num-lock+
                #:key-event #:key-event-key #:key-event-modifiers #:key-event-kind
                #:key-event-shifted-key #:key-event-base-layout-key #:key-event-text
                #:key-event-keypad
                #:mouse-event #:mouse-event-kind #:mouse-event-button #:mouse-event-x
                #:mouse-event-y #:mouse-event-modifiers
                #:paste-event #:paste-event-text #:focus-event #:focus-event-focused
                #:resize-event #:resize-event-width #:resize-event-height
                #:unknown-event #:unknown-event-bytes
                #:kitty-keyboard-reply #:kitty-keyboard-reply-flags
                #:mode-report #:mode-report-mode #:mode-report-value #:mode-report-private
                #:device-attributes-reply #:device-attributes-reply-params)
  (:export #:describe-event))

(in-package #:tui/input/describe)

(defconstant +paste-preview+ 60
  "Characters of a paste shown before it is cut off.")

(defun modifier-names (modifiers)
  "The names of the bits set in MODIFIERS."
  (loop for (bit name) in `((,+ctrl+ "ctrl") (,+alt+ "alt") (,+shift+ "shift")
                            (,+super+ "super") (,+hyper+ "hyper") (,+meta+ "meta")
                            (,+caps-lock+ "caps-lock") (,+num-lock+ "num-lock"))
        when (logtest bit modifiers)
          collect name))

(defun key-name (key)
  "KEY as written in a description: a character as itself (space as \"space\",
other invisible characters as U+XXXX), a named key as its keyword."
  (etypecase key
    (keyword (format nil "~(~S~)" key))
    (character (cond ((char= key #\Space) "space")
                     ((graphic-char-p key) (string key))
                     (t (format nil "U+~4,'0X" (char-code key)))))))

(defun quoted-text (text)
  "TEXT in double quotes, with control characters shown in caret notation (^J,
^[, ^?) so that printing a description can't control a terminal."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for char across text
          for code = (char-code char)
          do (cond ((< code 32) (write-char #\^ out) (write-char (code-char (+ code 64)) out))
                   ((= code 127) (write-string "^?" out))
                   ((<= 128 code 159) (format out "<U+~4,'0X>" code))
                   ((member char '(#\" #\\)) (write-char #\\ out) (write-char char out))
                   (t (write-char char out))))
    (write-char #\" out)))

(defun describe-key (event)
  (format nil "key ~{~A+~}~A ~(~A~)~@[ shifted=~A~]~@[ base=~A~]~@[ text=~A~]~:[~; keypad~]"
          (modifier-names (key-event-modifiers event))
          (key-name (key-event-key event))
          (key-event-kind event)
          (and (key-event-shifted-key event) (key-name (key-event-shifted-key event)))
          (and (key-event-base-layout-key event) (key-name (key-event-base-layout-key event)))
          (and (key-event-text event) (quoted-text (key-event-text event)))
          (key-event-keypad event)))

(defun describe-mouse (event)
  (format nil "mouse ~(~A~)~@[ ~(~A~)~] ~D,~D~@[ ~{~A~^+~}~]"
          (mouse-event-kind event)
          (mouse-event-button event)
          (mouse-event-x event)
          (mouse-event-y event)
          (modifier-names (mouse-event-modifiers event))))

(defun describe-paste (event)
  (let ((text (paste-event-text event)))
    (if (<= (length text) +paste-preview+)
        (format nil "paste ~A" (quoted-text text))
        (format nil "paste ~A... (~D characters)"
                (quoted-text (subseq text 0 +paste-preview+)) (length text)))))

(defun describe-event (event)
  "Return a short human-readable description of EVENT, for showing events to a
person (as the event inspector does), such as \"key ctrl+alt+a press\",
\"mouse drag left 10,4 ctrl\" or \"unknown #(27 91 57 57 120)\"."
  (etypecase event
    (key-event (describe-key event))
    (mouse-event (describe-mouse event))
    (paste-event (describe-paste event))
    (focus-event (if (focus-event-focused event) "focus in" "focus out"))
    (resize-event (format nil "resize ~Dx~D"
                          (resize-event-width event) (resize-event-height event)))
    (unknown-event (format nil "unknown #(~{~D~^ ~})"
                           (coerce (unknown-event-bytes event) 'list)))
    (kitty-keyboard-reply (format nil "kitty-keyboard-reply flags=~D"
                                  (kitty-keyboard-reply-flags event)))
    (mode-report (format nil "mode-report ~:[~;?~]~D value=~D"
                         (mode-report-private event)
                         (mode-report-mode event)
                         (mode-report-value event)))
    (device-attributes-reply (format nil "device-attributes-reply ~{~D~^;~}"
                                     (device-attributes-reply-params event)))))
