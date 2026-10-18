;;;; keys.lisp — tables mapping key numbers and bytes in terminal input to keys.

(defpackage #:tui/input/keys
  (:use #:cl)
  (:import-from #:tui/input/event #:+ctrl+)
  (:export #:c0-key #:letter-key #:tilde-key #:code-key))

(in-package #:tui/input/keys)

(defun c0-key (byte)
  "Return the key and modifiers that the legacy control byte BYTE (below 32, or
127) stands for."
  (declare (type (or (integer 0 31) (eql 127)) byte))
  (cond ((= byte 0) (values #\Space +ctrl+))
        ((= byte 9) (values :tab 0))
        ((= byte 13) (values :enter 0))
        ((= byte 27) (values :escape 0))
        ((= byte 127) (values :backspace 0))
        ((<= 1 byte 26) (values (code-char (+ byte 96)) +ctrl+))
        (t (values (code-char (+ byte 24)) +ctrl+))))

(defun function-key (n)
  "Return the keyword for function key N, from 1 to 35."
  (declare (type (integer 1 35) n))
  (svref #(nil :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12 :f13 :f14 :f15 :f16 :f17 :f18
           :f19 :f20 :f21 :f22 :f23 :f24 :f25 :f26 :f27 :f28 :f29 :f30 :f31 :f32 :f33 :f34 :f35)
         n))

(defun letter-key (final)
  "Return the key for the final character FINAL of a CSI or SS3 key sequence,
and whether it is a keypad key; nil when FINAL names no key."
  (declare (type character final))
  (case final
    (#\A :up)
    (#\B :down)
    (#\C :right)
    (#\D :left)
    (#\E (values :keypad-begin t))
    (#\F :end)
    (#\H :home)
    (#\P :f1)
    (#\Q :f2)
    (#\R :f3)
    (#\S :f4)))

(defun tilde-key (n)
  "Return the key for the legacy sequence CSI N ~, or nil."
  (declare (type fixnum n))
  (cond ((or (= n 1) (= n 7)) :home)
        ((= n 2) :insert)
        ((= n 3) :delete)
        ((or (= n 4) (= n 8)) :end)
        ((= n 5) :page-up)
        ((= n 6) :page-down)
        ((<= 11 n 15) (function-key (- n 10)))
        ((<= 17 n 21) (function-key (- n 11)))
        ((<= 23 n 26) (function-key (- n 12)))
        ((<= 28 n 29) (function-key (- n 13)))
        ((<= 31 n 34) (function-key (- n 14)))))

(defun keypad-key (code)
  "Return the key for the kitty keypad key number CODE, or nil."
  (declare (type fixnum code))
  (case code
    (57399 #\0) (57400 #\1) (57401 #\2) (57402 #\3) (57403 #\4)
    (57404 #\5) (57405 #\6) (57406 #\7) (57407 #\8) (57408 #\9)
    (57409 #\.) (57410 #\/) (57411 #\*) (57412 #\-) (57413 #\+)
    (57414 :enter) (57415 #\=) (57416 #\,)
    (57417 :left) (57418 :right) (57419 :up) (57420 :down)
    (57421 :page-up) (57422 :page-down) (57423 :home) (57424 :end)
    (57425 :insert) (57426 :delete) (57427 :keypad-begin)))

(defun functional-key (code)
  "Return the key for the kitty private-use key number CODE that isn't on the
keypad, or nil."
  (declare (type fixnum code))
  (case code
    (57358 :caps-lock) (57359 :scroll-lock) (57360 :num-lock)
    (57361 :print-screen) (57362 :pause) (57363 :menu)
    (57428 :media-play) (57429 :media-pause) (57430 :media-play-pause)
    (57431 :media-reverse) (57432 :media-stop) (57433 :media-fast-forward)
    (57434 :media-rewind) (57435 :media-track-next) (57436 :media-track-previous)
    (57437 :media-record) (57438 :lower-volume) (57439 :raise-volume) (57440 :mute-volume)
    (57441 :left-shift) (57442 :left-control) (57443 :left-alt)
    (57444 :left-super) (57445 :left-hyper) (57446 :left-meta)
    (57447 :right-shift) (57448 :right-control) (57449 :right-alt)
    (57450 :right-super) (57451 :right-hyper) (57452 :right-meta)
    (57453 :iso-level3-shift) (57454 :iso-level5-shift)
    (t (when (<= 57376 code 57398)
         (function-key (- code 57363))))))

(defun code-key (code)
  "Return the key for the key number CODE of a kitty (CSI u) or xterm
modifyOtherKeys sequence, and whether it is a keypad key; nil when CODE is not a
key."
  (declare (type fixnum code))
  (cond ((= code 27) :escape)
        ((= code 13) :enter)
        ((= code 9) :tab)
        ((= code 127) :backspace)
        ((<= 57399 code 57427) (values (keypad-key code) t))
        ((<= 57344 code 63743) (functional-key code))
        ((or (< code 0) (>= code char-code-limit) (<= #xd800 code #xdfff)) nil)
        (t (code-char code))))
