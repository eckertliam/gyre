;;;; input.lisp — the public API of the input decoder: events, the decoder and descriptions.

(uiop:define-package #:tui/input
  (:use-reexport #:tui/input/event
                 #:tui/input/decoder
                 #:tui/input/describe))
