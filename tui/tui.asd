;;;; tui.asd — system definitions for tui, a terminal UI library.

(defsystem "tui"
  :description "A terminal UI library in the style of ratatui."
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("tui/all")
  :in-order-to ((test-op (test-op "tui/tests"))))

(defsystem "tui/tests"
  :description "Tests and benchmarks for tui."
  :depends-on ("tui" "tui/examples" "sb-posix")
  :pathname "t"
  :serial t
  :components ((:file "harness")
               (:file "pty")
               (:file "screen")
               (:file "style")
               (:file "width")
               (:file "text")
               (:file "wrap")
               (:file "reflow")
               (:file "layout")
               (:file "layout-letters")
               (:file "layout-flex")
               (:file "layout-properties")
               (:file "rect")
               (:file "buffer")
               (:file "buffer-text")
               (:file "input")
               (:file "input-keys")
               (:file "input-events")
               (:file "input-long")
               (:file "terminal")
               (:file "caps")
               (:file "render")
               (:file "widgets")
               (:file "widgets-block")
               (:file "widgets-paragraph")
               (:file "widgets-list")
               (:file "widgets-table")
               (:file "widgets-tabs")
               (:file "widgets-alloc")
               (:file "examples")
               (:file "bench"))
  :perform (test-op (o c) (uiop:symbol-call '#:tui/tests/harness '#:run-tests)))

(defsystem "tui/examples"
  :description "Example programs for tui."
  :depends-on ("tui")
  :pathname "examples"
  :components ((:file "inspector")
               (:file "demo")))
