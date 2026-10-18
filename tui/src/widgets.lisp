;;;; widgets.lisp — the public API of the widgets: the widget protocol and the built-in
;;;; widgets.
;;;;
;;;; BLOCK and LIST are also CL symbols, so a package that uses both CL and TUI has to shadow
;;;; them (or refer to TUI:BLOCK and TUI:LIST explicitly).

(uiop:define-package #:tui/widgets
  (:use-reexport #:tui/widgets/widget
                 #:tui/widgets/text
                 #:tui/widgets/block
                 #:tui/widgets/paragraph
                 #:tui/widgets/list-state
                 #:tui/widgets/list
                 #:tui/widgets/table-row
                 #:tui/widgets/table-state
                 #:tui/widgets/table
                 #:tui/widgets/tabs
                 #:tui/widgets/scrollbar
                 #:tui/widgets/clear))
