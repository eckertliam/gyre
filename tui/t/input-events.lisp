;;;; input-events.lisp — decoder tables for everything but keys: mouse, bracketed
;;;; paste, focus, replies to feature queries, and input the decoder doesn't know.

(defpackage #:tui/tests/input-events
  (:use #:cl)
  (:import-from #:tui/tests/input
                #:bytes #:key #:mouse #:paste #:unknown #:row #:define-decoder-table)
  (:import-from #:tui/input
                #:make-focus-event #:make-kitty-keyboard-reply #:make-mode-report
                #:make-device-attributes-reply))

(in-package #:tui/tests/input-events)

(defun csi (&rest parts)
  "The bytes of ESC [ followed by PARTS."
  (apply #'bytes :esc "[" parts))

(define-decoder-table sgr-mouse
  (row (csi "<0;1;1M") (mouse :press :left 0 0))
  (row (csi "<1;10;5M") (mouse :press :middle 9 4))
  (row (csi "<2;300;200M") (mouse :press :right 299 199))
  (row (csi "<0;10;5m") (mouse :release :left 9 4))
  (row (csi "<2;10;5m") (mouse :release :right 9 4))
  (row (csi "<32;10;5M") (mouse :drag :left 9 4))
  (row (csi "<33;10;5M") (mouse :drag :middle 9 4))
  (row (csi "<34;10;5M") (mouse :drag :right 9 4))
  (row (csi "<35;10;5M") (mouse :motion nil 9 4))
  (row (csi "<64;10;5M") (mouse :scroll-up nil 9 4))
  (row (csi "<65;10;5M") (mouse :scroll-down nil 9 4))
  (row (csi "<66;10;5M") (mouse :scroll-left nil 9 4))
  (row (csi "<67;10;5M") (mouse :scroll-right nil 9 4))
  (row (csi "<4;1;1M") (mouse :press :left 0 0 :shift))
  (row (csi "<8;1;1M") (mouse :press :left 0 0 :alt))
  (row (csi "<16;1;1M") (mouse :press :left 0 0 :ctrl))
  (row (csi "<28;1;1M") (mouse :press :left 0 0 :shift :alt :ctrl))
  (row (csi "<48;3;4M") (mouse :drag :left 2 3 :ctrl))
  (row (csi "<81;1;1M") (mouse :scroll-down nil 0 0 :ctrl))
  (row (csi "<39;1;1M") (mouse :motion nil 0 0 :shift))
  (row (csi "<128;1;1M") (mouse :press :button-8 0 0))
  (row (csi "<129;1;1M") (mouse :press :button-9 0 0))
  (row (csi "<130;1;1M") (mouse :press :button-10 0 0))
  (row (csi "<131;1;1M") (mouse :press :button-11 0 0))
  (row (csi "<128;1;1m") (mouse :release :button-8 0 0))
  (row (csi "<160;1;1M") (mouse :drag :button-8 0 0))
  (row (csi "<0;0;0M") (mouse :press :left 0 0))
  (row (csi "<192;1;1M") (unknown :esc "[<192;1;1M"))
  (row (csi "<0;1M") (unknown :esc "[<0;1M"))
  (row (csi "<0;1;1x") (unknown :esc "[<0;1;1x")))

(define-decoder-table x10-and-urxvt-mouse
  (row (bytes :esc "[M" 32 33 33) (mouse :press :left 0 0))
  (row (bytes :esc "[M" 33 42 38) (mouse :press :middle 9 5))
  (row (bytes :esc "[M" 34 255 200) (mouse :press :right 222 167))
  (row (bytes :esc "[M" 35 42 38) (mouse :release nil 9 5))
  (row (bytes :esc "[M" 64 42 38) (mouse :drag :left 9 5))
  (row (bytes :esc "[M" 67 42 38) (mouse :motion nil 9 5))
  (row (bytes :esc "[M" 96 33 33) (mouse :scroll-up nil 0 0))
  (row (bytes :esc "[M" 97 33 33) (mouse :scroll-down nil 0 0))
  (row (bytes :esc "[M" 48 33 33) (mouse :press :left 0 0 :ctrl))
  ;; The three bytes after CSI M are raw, whatever they are.
  (row (bytes :esc "[M" 32 27 91) (mouse :press :left 0 58))
  (row (bytes :esc "[M" 31 33 33) (unknown :esc "[M" 31 33 33))
  (row (csi "32;10;5M") (mouse :press :left 9 4))
  (row (csi "35;10;5M") (mouse :release nil 9 4))
  (row (csi "96;10;5M") (mouse :scroll-up nil 9 4))
  (row (csi "31;10;5M") (unknown :esc "[31;10;5M")))

(define-decoder-table bracketed-paste
  (row (csi "200~hello" :esc "[201~") (paste "hello"))
  (row (csi "200~" :esc "[201~") (paste ""))
  (row (csi "200~a" (string #\Return) "b" (string #\Newline) "c" (string #\Tab) :esc "[201~")
       (paste (coerce (list #\a #\Return #\b #\Newline #\c #\Tab) 'string)))
  (row (csi "200~" (string (code-char 233)) (string (code-char #x4e2d))
            (string (code-char #x1f600)) :esc "[201~")
       (paste (coerce (list (code-char 233) (code-char #x4e2d) (code-char #x1f600)) 'string)))
  (row (csi "200~a" :esc "b" :esc "[A" :esc "[200~" :esc "[201x" :esc "[20" :esc :esc "[201~")
       (paste (concatenate 'string "a" (string (code-char 27)) "b"
                           (string (code-char 27)) "[A"
                           (string (code-char 27)) "[200~"
                           (string (code-char 27)) "[201x"
                           (string (code-char 27)) "[20"
                           (string (code-char 27)))))
  (row (csi "200~a" #xff "b" #xe2 #x82 :esc "[201~")
       (paste (coerce (list #\a (code-char #xfffd) #\b (code-char #xfffd)) 'string)))
  (row (csi "200~x" :esc "[201~y") (paste "x") (key #\y))
  (row (bytes "a" :esc "[200~b" :esc "[201~c") (key #\a) (paste "b") (key #\c))
  (row (bytes :esc :esc "[200~b" :esc "[201~") (key :escape) (paste "b"))
  (row (csi "200~" (make-string 5000 :initial-element #\z) :esc "[201~")
       (paste (make-string 5000 :initial-element #\z))))

(define-decoder-table focus
  (row (csi "I") (make-focus-event :focused t))
  (row (csi "O") (make-focus-event :focused nil))
  (row (csi "I" :esc "[O") (make-focus-event :focused t) (make-focus-event :focused nil))
  (row (csi "1I") (unknown :esc "[1I")))

(define-decoder-table feature-replies
  (row (csi "?0u") (make-kitty-keyboard-reply :flags 0))
  (row (csi "?u") (make-kitty-keyboard-reply :flags 0))
  (row (csi "?5u") (make-kitty-keyboard-reply :flags 5))
  (row (csi "?31u") (make-kitty-keyboard-reply :flags 31))
  (row (csi "?2026;0$y") (make-mode-report :mode 2026 :value 0 :private t))
  (row (csi "?2026;1$y") (make-mode-report :mode 2026 :value 1 :private t))
  (row (csi "?2026;2$y") (make-mode-report :mode 2026 :value 2 :private t))
  (row (csi "?2026;3$y") (make-mode-report :mode 2026 :value 3 :private t))
  (row (csi "?2026;4$y") (make-mode-report :mode 2026 :value 4 :private t))
  (row (csi "4;2$y") (make-mode-report :mode 4 :value 2 :private nil))
  (row (csi "?62;22c") (make-device-attributes-reply :params '(62 22)))
  (row (csi "?1;2c") (make-device-attributes-reply :params '(1 2)))
  (row (csi "?64;1;2;6;9;15;16;17;18;21;22;28c")
       (make-device-attributes-reply :params '(64 1 2 6 9 15 16 17 18 21 22 28)))
  (row (csi "?c") (make-device-attributes-reply :params '()))
  ;; A complete detection exchange arriving in one read.
  (row (csi "?1u" :esc "[?2026;2$y" :esc "[?62;22c")
       (make-kitty-keyboard-reply :flags 1)
       (make-mode-report :mode 2026 :value 2 :private t)
       (make-device-attributes-reply :params '(62 22)))
  (row (csi "?2026$y") (unknown :esc "[?2026$y"))
  (row (csi ">1;10;0c") (unknown :esc "[>1;10;0c")))

(define-decoder-table unknown-sequences
  (row (csi "99x") (unknown :esc "[99x"))
  (row (csi "1;2;3 q") (unknown :esc "[1;2;3 q"))
  (row (csi "?1;2x") (unknown :esc "[?1;2x"))
  (row (csi "=1u") (unknown :esc "[=1u"))
  (row (csi "1?u") (unknown :esc "[1?u"))
  (row (csi "1$$y") (unknown :esc "[1$$y"))
  (row (csi "[A") (unknown :esc "[[") (key #\A))
  (row (bytes :esc "]11;rgb:1a1a/1a1a/1a1a" 7) (unknown :esc "]11;rgb:1a1a/1a1a/1a1a" 7))
  (row (bytes :esc "]11;rgb:0/0/0" :esc "\\") (unknown :esc "]11;rgb:0/0/0" :esc "\\"))
  (row (bytes :esc "]" (string (code-char 233)) 7) (unknown :esc "]" #xc3 #xa9 7))
  (row (bytes :esc "P1$r0m" :esc "\\") (unknown :esc "P1$r0m" :esc "\\"))
  (row (bytes :esc "P>|kitty(0.40)" :esc "\\") (unknown :esc "P>|kitty(0.40)" :esc "\\"))
  (row (bytes :esc "P" 7 :esc "\\") (key #\P :alt) (key #\g :ctrl) (key #\\ :alt))
  (row (bytes :esc "_Gi=1;OK" :esc "\\") (unknown :esc "_Gi=1;OK" :esc "\\"))
  (row (bytes :esc "X") (key #\X :alt))
  (row (bytes :esc "^") (key #\^ :alt))
  (row (bytes #x80) (unknown #x80))
  (row (bytes #xff "a") (unknown #xff) (key #\a))
  (row (bytes #xc0 #xaf) (unknown #xc0) (unknown #xaf))
  (row (bytes #xe0 #x80 #x80) (unknown #xe0) (unknown #x80) (unknown #x80))
  (row (bytes #xed #xa0 #x80) (unknown #xed) (unknown #xa0) (unknown #x80))
  (row (bytes #xf4 #x90 #x80 #x80) (unknown #xf4) (unknown #x90) (unknown #x80) (unknown #x80))
  (row (bytes #xe2 #x82 "x") (unknown #xe2 #x82) (key #\x))
  (row (bytes #xe2 :esc "[A") (unknown #xe2) (key :up)))

(define-decoder-table malformed-sequences
  ;; A byte that can't continue the sequence ends it and is decoded on its own.
  (row (bytes :esc "[" 1) (key #\[ :alt) (key #\a :ctrl))
  (row (bytes :esc "[" :esc "[A") (key #\[ :alt) (key :up))
  (row (bytes :esc "[" (string (code-char 233))) (key #\[ :alt) (key (code-char 233)))
  (row (bytes :esc "[1" 13) (unknown :esc "[1") (key :enter))
  (row (bytes :esc "[1;5" :esc "[B") (unknown :esc "[1;5") (key :down))
  (row (bytes :esc "[1$2") (unknown :esc "[1$") (key #\2))
  (row (bytes :esc "O" 1) (key #\O :alt) (key #\a :ctrl))
  (row (bytes :esc "O1") (key #\O :alt) (key #\1))
  (row (bytes :esc "]11;x" 13) (unknown :esc "]11;x") (key :enter))
  (row (bytes :esc "]" 13) (key #\] :alt) (key :enter))
  (row (bytes :esc "]abc" :esc "[A") (unknown :esc "]abc") (key :up))
  (row (bytes :esc "_" 127) (key #\_ :alt) (key :backspace))
  ;; A CSI sequence longer than 256 bytes.
  (let ((digits (make-string 300 :initial-element #\1)))
    (apply #'row (bytes :esc "[" digits "A")
           (unknown :esc "[" (subseq digits 0 254))
           (append (loop repeat 46 collect (key #\1))
                   (list (key #\A)))))
  ;; An OSC string longer than the decoder's buffer arrives in pieces.
  (let ((text (make-string 5000 :initial-element #\x)))
    (row (bytes :esc "]" text 7)
         (unknown :esc "]" (subseq text 0 4093))
         (unknown (subseq text 4093) 7))))
