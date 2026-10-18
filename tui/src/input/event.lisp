;;;; event.lisp — the events the input decoder produces, and modifier bits.

(defpackage #:tui/input/event
  (:use #:cl)
  (:export #:+shift+ #:+alt+ #:+ctrl+ #:+super+ #:+hyper+ #:+meta+ #:+caps-lock+ #:+num-lock+
           #:event #:event-p
           #:key-event #:make-key-event #:key-event-p #:key-event-key #:key-event-modifiers
           #:key-event-kind #:key-event-shifted-key #:key-event-base-layout-key
           #:key-event-text #:key-event-keypad #:key-event=
           #:mouse-event #:make-mouse-event #:mouse-event-p #:mouse-event-kind
           #:mouse-event-button #:mouse-event-x #:mouse-event-y #:mouse-event-modifiers
           #:paste-event #:make-paste-event #:paste-event-p #:paste-event-text
           #:focus-event #:make-focus-event #:focus-event-p #:focus-event-focused
           #:resize-event #:make-resize-event #:resize-event-p
           #:resize-event-width #:resize-event-height
           #:unknown-event #:make-unknown-event #:unknown-event-p #:unknown-event-bytes
           #:kitty-keyboard-reply #:make-kitty-keyboard-reply #:kitty-keyboard-reply-p
           #:kitty-keyboard-reply-flags
           #:mode-report #:make-mode-report #:mode-report-p
           #:mode-report-mode #:mode-report-value #:mode-report-private
           #:device-attributes-reply #:make-device-attributes-reply #:device-attributes-reply-p
           #:device-attributes-reply-params))

(in-package #:tui/input/event)

;;; Modifier bits, laid out as in the kitty keyboard protocol (whose modifier
;;; field on the wire is this mask plus one).

(defconstant +shift+ 1)
(defconstant +alt+ 2)
(defconstant +ctrl+ 4)
(defconstant +super+ 8)
(defconstant +hyper+ 16)
(defconstant +meta+ 32)
(defconstant +caps-lock+ 64)
(defconstant +num-lock+ 128)

;;; Events

(defstruct (event (:constructor nil) (:copier nil))
  "Base type of everything the input decoder returns, and of RESIZE-EVENT.")

(defstruct (key-event (:include event) (:copier nil))
  "A key press, repeat or release.
KEY is a character for keys that produce text (#\\Space for space) or a keyword
naming any other key (:enter, :up, :f5, :left-shift ...). MODIFIERS is a mask of
+SHIFT+, +ALT+, +CTRL+, +SUPER+, +HYPER+, +META+, +CAPS-LOCK+ and +NUM-LOCK+.
Shift is folded into character keys whenever the shifted character is known, so
shift+a is #\\A with no modifiers, as typed without the kitty protocol. KIND is
:PRESS, :REPEAT or :RELEASE. SHIFTED-KEY, BASE-LAYOUT-KEY and TEXT are the kitty
protocol's alternate keys and associated text, nil when the terminal didn't send
them. KEYPAD is true for keys the kitty protocol reports as on the keypad."
  (key nil :type (or character keyword))
  (modifiers 0 :type fixnum)
  (kind :press :type (member :press :repeat :release))
  (shifted-key nil :type (or null character keyword))
  (base-layout-key nil :type (or null character keyword))
  (text nil :type (or null string))
  (keypad nil :type boolean))

(defstruct (mouse-event (:include event) (:copier nil))
  "A mouse action at column X and row Y, both counted from 0.
KIND is :PRESS, :RELEASE, :DRAG, :MOTION, :SCROLL-UP, :SCROLL-DOWN, :SCROLL-LEFT
or :SCROLL-RIGHT. BUTTON is :LEFT, :MIDDLE, :RIGHT, :BUTTON-8 to :BUTTON-11, or
nil when no button is involved or the terminal didn't say which one was
released. MODIFIERS is a mask of +SHIFT+, +ALT+ and +CTRL+."
  (kind nil :type (member :press :release :drag :motion
                          :scroll-up :scroll-down :scroll-left :scroll-right))
  (button nil :type (member nil :left :middle :right
                            :button-8 :button-9 :button-10 :button-11))
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (modifiers 0 :type fixnum))

(defstruct (paste-event (:include event) (:copier nil))
  "A bracketed paste. TEXT is the pasted bytes decoded as UTF-8, verbatim, with
invalid sequences replaced by U+FFFD."
  (text nil :type string))

(defstruct (focus-event (:include event) (:copier nil))
  "The terminal gained (FOCUSED true) or lost focus."
  (focused nil :type boolean))

(defstruct (resize-event (:include event) (:copier nil))
  "The terminal is now WIDTH columns by HEIGHT rows. The decoder never produces
this; an application constructs it to put resizes into its own event stream."
  (width 0 :type fixnum)
  (height 0 :type fixnum))

(defstruct (unknown-event (:include event) (:copier nil))
  "Input the decoder didn't recognize. BYTES are the exact bytes received."
  (bytes nil :type (simple-array (unsigned-byte 8) (*))))

(defstruct (kitty-keyboard-reply (:include event) (:copier nil))
  "The terminal's answer to CSI ? u: the kitty keyboard protocol FLAGS in effect."
  (flags 0 :type fixnum))

(defstruct (mode-report (:include event) (:copier nil))
  "The terminal's answer to DECRQM: MODE's VALUE is 0 (not recognized), 1 (set),
2 (reset), 3 (permanently set) or 4 (permanently reset). PRIVATE is true for a
DEC private mode (CSI ? mode ; value $ y)."
  (mode 0 :type fixnum)
  (value 0 :type fixnum)
  (private nil :type boolean))

(defstruct (device-attributes-reply (:include event) (:copier nil))
  "The terminal's answer to primary device attributes (CSI c): the numeric
PARAMS of CSI ? params c, in order."
  (params nil :type list))

(defun key-event= (a b)
  "True when key events A and B have the same key, modifiers and kind. The kitty
extras (shifted key, base layout key, text and keypad) are not compared."
  (declare (type key-event a b))
  (and (eql (key-event-key a) (key-event-key b))
       (= (key-event-modifiers a) (key-event-modifiers b))
       (eq (key-event-kind a) (key-event-kind b))))
