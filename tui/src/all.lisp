;;;; all.lisp — the public package TUI, which re-exports the public API of every module.

(uiop:define-package #:tui/all
  (:nicknames #:tui)
  (:use-reexport #:tui/style
                 #:tui/width
                 #:tui/text
                 #:tui/text/range
                 #:tui/text/truncate
                 #:tui/text/wrap
                 #:tui/layout
                 #:tui/buffer
                 #:tui/buffer/text
                 #:tui/input
                 #:tui/terminal
                 #:tui/caps
                 #:tui/render
                 #:tui/widgets))
