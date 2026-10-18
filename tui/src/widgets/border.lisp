;;;; border.lisp — the box-drawing characters of block borders, as in ratatui's symbols::border.
;;;; Not part of the public API.

(defpackage #:tui/widgets/border
  (:use #:cl)
  (:export #:border-type
           #:border-set
           #:border-set-top-left
           #:border-set-top-right
           #:border-set-bottom-left
           #:border-set-bottom-right
           #:border-set-vertical-left
           #:border-set-vertical-right
           #:border-set-horizontal-top
           #:border-set-horizontal-bottom
           #:find-border-set))

(in-package #:tui/widgets/border)

(deftype border-type ()
  "The kinds of border a block can have."
  '(member :plain :rounded :double :thick))

(defstruct (border-set (:constructor make-border-set
                           (top-left top-right bottom-left bottom-right vertical-left
                            vertical-right horizontal-top horizontal-bottom))
                       (:copier nil))
  "The characters, each a one-grapheme string, that draw the corners and sides of a border."
  (top-left "" :type simple-string :read-only t)
  (top-right "" :type simple-string :read-only t)
  (bottom-left "" :type simple-string :read-only t)
  (bottom-right "" :type simple-string :read-only t)
  (vertical-left "" :type simple-string :read-only t)
  (vertical-right "" :type simple-string :read-only t)
  (horizontal-top "" :type simple-string :read-only t)
  (horizontal-bottom "" :type simple-string :read-only t))

(defun box (top-left top-right bottom-left bottom-right vertical horizontal)
  (flet ((string-of (code) (string (code-char code))))
    (let ((vertical (string-of vertical))
          (horizontal (string-of horizontal)))
      (make-border-set (string-of top-left) (string-of top-right) (string-of bottom-left)
                       (string-of bottom-right) vertical vertical horizontal horizontal))))

(defparameter *border-sets*
  (list (cons :plain (box #x250C #x2510 #x2514 #x2518 #x2502 #x2500))     ; ┌┐└┘│─
        (cons :rounded (box #x256D #x256E #x2570 #x256F #x2502 #x2500))   ; ╭╮╰╯│─
        (cons :double (box #x2554 #x2557 #x255A #x255D #x2551 #x2550))    ; ╔╗╚╝║═
        (cons :thick (box #x250F #x2513 #x2517 #x251B #x2503 #x2501)))    ; ┏┓┗┛┃━
  "The border set of each border type.")

(defun find-border-set (border-type)
  "Return the border set that draws borders of BORDER-TYPE."
  (check-type border-type border-type)
  (cdr (assoc border-type *border-sets*)))
