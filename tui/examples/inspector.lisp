;;;; inspector.lisp — the event inspector: shows the raw bytes the terminal sends and the
;;;; events tui decodes from them. Run it with `make inspector` inside tmux, Ghostty or
;;;; kitty to check input handling by hand.
;;;;
;;;; Ctrl-C twice in a row quits; ctrl-z suspends (exercising SUSPEND and RESUME). The
;;;; example installs its own SIGWINCH handler; the library never does.

(defpackage #:tui/examples/inspector
  (:use #:cl)
  (:import-from #:tui
                #:make-terminal #:with-terminal #:draw #:frame-buffer #:frame-area
                #:set-string #:make-style #:rect-width #:rect-height #:make-decoder
                #:feed-decoder #:flush-decoder #:decoder-pending-p #:describe-event
                #:key-event-p #:key-event-key #:key-event-modifiers #:key-event-kind
                #:make-resize-event #:detect-features #:kitty-keyboard-supported-p
                #:synchronized-output-supported-p #:terminal-size #:wait-for-input
                #:read-input #:suspend #:resume #:+ctrl+)
  (:export #:main))

(in-package #:tui/examples/inspector)

(defparameter *title-style* (make-style :fg :black :bg :cyan :modifiers :bold))
(defparameter *bytes-style* (make-style :fg :dark-gray))
(defparameter *event-style* (make-style :fg :green))
(defparameter *note-style* (make-style :fg :yellow))

(defconstant +log-limit+ 1000)

(defstruct (inspector (:constructor make-inspector (terminal)))
  terminal
  (decoder (make-decoder))
  ;; Log entries, newest last: (style . text).
  (log (make-array 0 :adjustable t :fill-pointer t))
  (ctrl-c-count 0))

(defvar *resized* nil
  "Set by the SIGWINCH handler; the main loop turns it into a resize event.")

(defun log-entry (inspector style text)
  (let ((log (inspector-log inspector)))
    (when (>= (length log) +log-limit+)
      (replace log log :start2 1)
      (decf (fill-pointer log)))
    (vector-push-extend (cons style text) log)))

(defun printable-byte (byte)
  (cond ((= byte 27) "^[")
        ((< byte 32) (format nil "^~C" (code-char (+ byte 64))))
        ((= byte 127) "^?")
        ((< byte 127) (string (code-char byte)))
        (t "·")))

(defun bytes-text (octets count)
  (with-output-to-string (out)
    (format out "bytes ~D:" count)
    (dotimes (i count) (format out " ~2,'0x" (aref octets i)))
    (write-string "   " out)
    (dotimes (i count) (write-string (printable-byte (aref octets i)) out))))

(defun log-event (inspector event &optional note)
  (log-entry inspector *event-style*
             (format nil "  ~A~@[   (~A)~]" (describe-event event) note)))

(defun ctrl-c-p (event)
  (and (key-event-p event)
       (eql (key-event-key event) #\c)
       (= (key-event-modifiers event) +ctrl+)
       (eq (key-event-kind event) :press)))

(defun ctrl-z-p (event)
  (and (key-event-p event)
       (eql (key-event-key event) #\z)
       (= (key-event-modifiers event) +ctrl+)
       (eq (key-event-kind event) :press)))

(defun suspend-process (inspector)
  "Restore the terminal, stop like a shell job, and set the terminal up again when continued."
  (let ((terminal (inspector-terminal inspector)))
    (suspend terminal)
    (sb-posix:kill 0 sb-posix:sigtstp)
    (resume terminal)
    (log-entry inspector *note-style* "resumed after ctrl-z")))

(defun handle-event (inspector event &optional note)
  "Log EVENT. Return true when the inspector should quit."
  (log-event inspector event note)
  (cond ((ctrl-c-p event)
         (>= (incf (inspector-ctrl-c-count inspector)) 2))
        (t
         (setf (inspector-ctrl-c-count inspector) 0)
         (when (ctrl-z-p event)
           (suspend-process inspector))
         nil)))

;;; Drawing

(defun header-lines (inspector)
  (let ((terminal (inspector-terminal inspector)))
    (multiple-value-bind (columns rows) (terminal-size terminal)
      (list (cons *title-style* " tui event inspector — ctrl-c twice quits, ctrl-z suspends")
            (cons nil (format nil " kitty keyboard protocol: ~:[no~;yes~]   ~
                                    synchronized output: ~:[no~;yes~]   size: ~D×~D"
                              (kitty-keyboard-supported-p terminal)
                              (synchronized-output-supported-p terminal) columns rows))
            (cons nil "")))))

(defun render (inspector frame)
  (let* ((buffer (frame-buffer frame))
         (area (frame-area frame))
         (header (header-lines inspector))
         (log (inspector-log inspector))
         (rows (rect-height area)))
    (loop for (style . text) in header
          for y from 0
          do (set-string buffer 0 y text :style style))
    ;; Newest entries at the bottom.
    (let ((visible (max 0 (- rows (length header)))))
      (loop for y from (1- rows) downto (length header)
            for index downfrom (1- (length log))
            repeat visible
            while (>= index 0)
            do (destructuring-bind (style . text) (aref log index)
                 (set-string buffer 0 y text :style style))))))

;;; Main loop

(defun install-resize-handler ()
  (sb-sys:enable-interrupt sb-posix:sigwinch
                           (lambda (signal info context)
                             (declare (ignore signal info context))
                             (setf *resized* t))))

(defun read-and-decode (inspector octets)
  "Read the available input, log its bytes and handle each event. Return true to quit."
  (let* ((terminal (inspector-terminal inspector))
         (count (read-input terminal octets)))
    (when (plusp count)
      (log-entry inspector *bytes-style* (bytes-text octets count))
      (loop for event in (feed-decoder (inspector-decoder inspector) octets :end count)
            thereis (handle-event inspector event)))))

(defun step-loop (inspector octets)
  "Wait for something to happen and handle it. Return true to quit."
  (let* ((terminal (inspector-terminal inspector))
         (decoder (inspector-decoder inspector))
         ;; A lone ESC (or another ambiguous prefix) is held until 25 ms pass without more
         ;; input; then it is flushed as Esc or Alt+key.
         (timeout (if (decoder-pending-p decoder) 0.025 nil)))
    (cond ((wait-for-input terminal timeout)
           (read-and-decode inspector octets))
          (*resized*
           (setf *resized* nil)
           (multiple-value-bind (columns rows) (terminal-size terminal)
             (handle-event inspector (make-resize-event :width columns :height rows)
                           "from SIGWINCH")))
          ((decoder-pending-p decoder)
           (loop for event in (flush-decoder decoder)
                 thereis (handle-event inspector event "flushed after 25 ms"))))))

(defun main (&key (input-fd 0) (output-fd 1) (resize-handler t))
  "Run the event inspector on the terminal at INPUT-FD and OUTPUT-FD (by default the
process's own). With RESIZE-HANDLER, install a SIGWINCH handler to log resizes."
  (finish-output)
  (let* ((terminal (make-terminal input-fd output-fd))
         (inspector (make-inspector terminal))
         (octets (make-array 4096 :element-type '(unsigned-byte 8))))
    (when resize-handler
      (install-resize-handler))
    (with-terminal (terminal :mouse t)
      (dolist (event (detect-features terminal (inspector-decoder inspector)))
        (handle-event inspector event "arrived during feature detection"))
      (loop (draw terminal (lambda (frame) (render inspector frame)))
            (when (step-loop inspector octets)
              (return))))))
