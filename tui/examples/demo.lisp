;;;; demo.lisp — the widget demo: a resizable layout using every built-in widget. Run it
;;;; with `make demo`.
;;;;
;;;; Tab and shift-tab switch between the List, Table and Text tabs, which choose what the
;;;; arrow keys (or j/k, or the mouse wheel) move: the list selection, the table selection or
;;;; the paragraph's scroll position. p toggles a popup, ctrl-z suspends, q or ctrl-c quits.
;;;; The example installs its own SIGWINCH handler; the library never does.

(defpackage #:tui/examples/demo
  (:use #:cl)
  (:import-from #:tui
                #:make-terminal #:with-terminal #:draw #:frame-area #:render-widget
                #:render-stateful-widget #:make-style #:make-line #:make-text #:span
                #:make-layout #:split #:make-rect #:rect-x #:rect-y #:rect-width #:rect-height
                #:rect-right #:paragraph #:table #:table-row #:tabs #:scrollbar #:clear
                #:make-list-state #:list-state-selected #:list-select-next
                #:list-select-previous #:make-table-state #:table-state-selected
                #:table-select-next #:table-select-previous #:make-scrollbar-state
                #:make-decoder #:feed-decoder #:flush-decoder #:decoder-pending-p
                #:key-event-p #:key-event-key #:key-event-modifiers #:key-event-kind
                #:mouse-event-p #:mouse-event-kind #:detect-features #:wait-for-input
                #:read-input #:suspend #:resume #:+ctrl+ #:+shift+)
  (:export #:main))

(in-package #:tui/examples/demo)

;;; Content

(defparameter *items*
  (coerce (loop for i from 1 to 40
                collect (format nil "Item ~D~:[~; — has a longer label that gets cut off~]"
                                i (zerop (mod i 7))))
          'vector))

(defparameter *languages*
  '(("Common Lisp" "1984" "SBCL") ("Rust" "2015" "rustc") ("OCaml" "1996" "ocamlopt")
    ("Haskell" "1990" "GHC") ("Scheme" "1975" "Chez") ("Zig" "2016" "zig")
    ("Erlang" "1986" "BEAM") ("Forth" "1970" "gforth") ("Smalltalk" "1972" "Pharo")
    ("Prolog" "1972" "SWI") ("Ada" "1983" "GNAT") ("APL" "1966" "Dyalog")))

(defun story-text ()
  (make-text
   (list (make-line (list (span "tui " (make-style :fg :cyan :modifiers :bold))
                          "is an immediate-mode terminal UI library in the style of "
                          (span "ratatui" (make-style :fg :yellow :modifiers :italic))
                          ". Every frame is rendered into a buffer from scratch; only the "
                          "cells that changed reach the terminal."))
         ""
         (make-line (list "Wide characters work: "
                          (span "日本語のテキスト" (make-style :fg :green))
                          ", emoji too: 😀 ❤️ 🇺🇸 👨‍👩‍👧."))
         ""
         (make-line (list "Styles: "
                          (span "bold" (make-style :modifiers :bold)) ", "
                          (span "dim" (make-style :modifiers :dim)) ", "
                          (span "italic" (make-style :modifiers :italic)) ", "
                          (span "curly underline" (make-style :modifiers :curly-underline
                                                              :underline-color :red))
                          ", "
                          (span "reverse" (make-style :modifiers :reverse)) ", "
                          (span "strikethrough" (make-style :modifiers :strikethrough))
                          ", and 24-bit "
                          (span "c" (make-style :fg "#ff5f5f"))
                          (span "o" (make-style :fg "#ffaf5f"))
                          (span "l" (make-style :fg "#ffff5f"))
                          (span "o" (make-style :fg "#5fff5f"))
                          (span "r" (make-style :fg "#5fafff"))
                          (span "s" (make-style :fg "#af5fff"))
                          "."))
         ""
         (concatenate 'string
                      "Resize the terminal: the layout follows. Lines wrap at word boundaries "
                      "and long words break between grapheme clusters. Switch to the Text tab "
                      "to scroll this paragraph with the arrow keys."))))

;;; State

(defstruct (demo (:constructor %make-demo))
  terminal
  (decoder (make-decoder))
  (tab 0)
  (list-state (make-list-state :selected 0))
  (table-state (make-table-state :selected 0))
  (scroll 0)
  (popup nil)
  ;; Widgets built once; only their state changes from frame to frame.
  list-widget table-widget story popup-widget help-widget
  (layouts nil))

(defvar *resized* nil
  "Set by the SIGWINCH handler; the next loop iteration redraws at the new size.")

(defun titled (title)
  (tui:block :borders :all :border-type :rounded :title (format nil " ~A " title)
             :border-style (make-style :fg :dark-gray)
             :title-style (make-style :fg :cyan :modifiers :bold)))

(defun make-widgets (demo)
  (let ((highlight (make-style :bg (tui:rgb 50 60 90) :modifiers :bold)))
    (setf (demo-list-widget demo)
          (tui:list *items* :block (titled "List") :highlight-style highlight
                            :highlight-symbol "> " :scroll-padding 2)
          (demo-table-widget demo)
          (table (mapcar #'table-row *languages*)
                 :widths '((:fill 2) (:length 6) (:fill 1))
                 :header (table-row '("Language" "Year" "Compiler")
                                    :style (make-style :fg :yellow :modifiers :bold))
                 :block (titled "Table") :row-highlight-style highlight
                 :highlight-symbol "» ")
          (demo-story demo) (story-text)
          (demo-popup-widget demo)
          (paragraph (concatenate 'string "This popup is drawn over the other widgets after a "
                                  "CLEAR widget resets the cells under it. Press p to close it.")
                     :wrap t :alignment :center
                     :block (tui:block :borders :all :border-type :double :title " Popup "
                                       :title-alignment :center
                                       :style (make-style :bg (tui:rgb 30 30 40))))
          (demo-help-widget demo)
          (make-line (list (span " tab " (make-style :fg :black :bg :cyan))
                           " switch  "
                           (span " ↑↓ jk " (make-style :fg :black :bg :cyan))
                           " move  "
                           (span " p " (make-style :fg :black :bg :cyan))
                           " popup  "
                           (span " ^z " (make-style :fg :black :bg :cyan))
                           " suspend  "
                           (span " q " (make-style :fg :black :bg :cyan))
                           " quit")))))

(defun make-demo (terminal)
  (let ((demo (%make-demo :terminal terminal)))
    (make-widgets demo)
    (setf (demo-layouts demo)
          (list :outer (make-layout :constraints '((:length 3) (:fill 1) (:length 1)))
                :body (make-layout :direction :horizontal
                                   :constraints '((:percentage 35) (:fill 1)))
                :right (make-layout :constraints '((:fill 1) (:length 9)))))
    demo))

(defun layout (demo name)
  (getf (demo-layouts demo) name))

;;; Drawing

(defun centered (area width-percent height-percent)
  (let ((width (floor (* (rect-width area) width-percent) 100))
        (height (floor (* (rect-height area) height-percent) 100)))
    (make-rect (+ (rect-x area) (floor (- (rect-width area) width) 2))
               (+ (rect-y area) (floor (- (rect-height area) height) 2))
               width height)))

(defun render-list (demo frame area)
  (render-stateful-widget frame (demo-list-widget demo) (demo-list-state demo) area)
  ;; A scrollbar on the list's right border.
  (render-stateful-widget frame (scrollbar)
                          (make-scrollbar-state :content-length (length *items*)
                                                :position (or (list-state-selected
                                                               (demo-list-state demo))
                                                              0)
                                                :viewport-length (- (rect-height area) 2))
                          (make-rect (1- (rect-right area)) (1+ (rect-y area))
                                     1 (max 0 (- (rect-height area) 2)))))

(defun render-text (demo frame area)
  (render-widget frame (paragraph (demo-story demo)
                                  :block (titled "Text") :wrap t
                                  :scroll-y (demo-scroll demo))
                 area))

(defun render-demo (demo frame)
  (destructuring-bind (top body bottom) (coerce (split (layout demo :outer) (frame-area frame))
                                                'list)
    (render-widget frame (tabs '("List" "Table" "Text") :selected (demo-tab demo)
                                                         :block (titled "tui demo")
                                                         :highlight-style
                                                         (make-style :fg :black :bg :cyan))
                   top)
    (destructuring-bind (left right) (coerce (split (layout demo :body) body) 'list)
      (render-list demo frame left)
      (destructuring-bind (upper lower) (coerce (split (layout demo :right) right) 'list)
        (render-text demo frame upper)
        (render-stateful-widget frame (demo-table-widget demo) (demo-table-state demo) lower)))
    (render-widget frame (demo-help-widget demo) bottom)
    (when (demo-popup demo)
      (let ((area (centered (frame-area frame) 60 40)))
        (render-widget frame (clear) area)
        (render-widget frame (demo-popup-widget demo) area)))))

;;; Input

(defun key-p (event key &optional (modifiers 0))
  (and (key-event-p event)
       (not (eq (key-event-kind event) :release))
       (eql (key-event-key event) key)
       (= (key-event-modifiers event) modifiers)))

(defun move (demo direction)
  "Move whatever the current tab controls by DIRECTION, 1 or -1."
  (ecase (demo-tab demo)
    (0 (if (plusp direction)
           (list-select-next (demo-list-state demo))
           (list-select-previous (demo-list-state demo))))
    (1 (if (plusp direction)
           (table-select-next (demo-table-state demo))
           (table-select-previous (demo-table-state demo))))
    (2 (setf (demo-scroll demo) (max 0 (+ (demo-scroll demo) direction))))))

(defun suspend-process (demo)
  (suspend (demo-terminal demo))
  (sb-posix:kill 0 sb-posix:sigtstp)
  (resume (demo-terminal demo)))

(defun handle-event (demo event)
  "Act on EVENT. Return true to quit."
  (cond ((or (key-p event #\q) (key-p event #\c +ctrl+)) t)
        ((key-p event #\z +ctrl+) (suspend-process demo) nil)
        ((key-p event :tab) (setf (demo-tab demo) (mod (1+ (demo-tab demo)) 3)) nil)
        ((key-p event :tab +shift+) (setf (demo-tab demo) (mod (1- (demo-tab demo)) 3)) nil)
        ((or (key-p event :down) (key-p event #\j)) (move demo 1) nil)
        ((or (key-p event :up) (key-p event #\k)) (move demo -1) nil)
        ((key-p event #\p) (setf (demo-popup demo) (not (demo-popup demo))) nil)
        ((and (mouse-event-p event) (eq (mouse-event-kind event) :scroll-down))
         (move demo 1) nil)
        ((and (mouse-event-p event) (eq (mouse-event-kind event) :scroll-up))
         (move demo -1) nil)
        (t nil)))

(defun next-events (demo octets)
  "Wait for input, a resize or the Esc timeout, and return the events that arrived."
  (let ((decoder (demo-decoder demo)))
    (cond ((wait-for-input (demo-terminal demo) (if (decoder-pending-p decoder) 0.025 nil))
           (feed-decoder decoder octets :end (read-input (demo-terminal demo) octets)))
          ((decoder-pending-p decoder)
           (flush-decoder decoder))
          (t
           (setf *resized* nil)
           '()))))

(defun install-resize-handler ()
  (sb-sys:enable-interrupt sb-posix:sigwinch
                           (lambda (signal info context)
                             (declare (ignore signal info context))
                             (setf *resized* t))))

(defun main (&key (input-fd 0) (output-fd 1) (resize-handler t))
  "Run the widget demo on the terminal at INPUT-FD and OUTPUT-FD (by default the process's
own). With RESIZE-HANDLER, install a SIGWINCH handler so that resizes redraw at once."
  (finish-output)
  (let* ((terminal (make-terminal input-fd output-fd))
         (demo (make-demo terminal))
         (octets (make-array 4096 :element-type '(unsigned-byte 8))))
    (when resize-handler
      (install-resize-handler))
    (with-terminal (terminal :mouse t)
      (dolist (event (detect-features terminal (demo-decoder demo)))
        (handle-event demo event))
      (loop (draw terminal (lambda (frame) (render-demo demo frame)))
            (when (some (lambda (event) (handle-event demo event))
                        (next-events demo octets))
              (return))))))
