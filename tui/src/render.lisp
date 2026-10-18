;;;; render.lisp — immediate-mode drawing with double buffering, as in ratatui.
;;;;
;;;; DRAW resets the current buffer, lets the application render a frame into it, writes the
;;;; difference from the previous frame to the terminal and swaps the buffers. In the steady
;;;; state (same size, no full redraw) drawing a frame allocates nothing.

(defpackage #:tui/render
  (:use #:cl)
  (:import-from #:tui/layout #:make-rect)
  (:import-from #:tui/buffer #:buffer #:make-buffer #:resize-buffer #:reset-buffer)
  (:import-from #:tui/cells #:buffer-width #:buffer-height)
  (:import-from #:tui/diff
                #:pen #:make-pen #:reset-pen #:pen-x #:pen-y #:move-cursor #:write-sgr-reset
                #:write-diff)
  (:import-from #:tui/output
                #:output #:output-csi #:output-byte #:output-decimal #:output-fill
                #:output-ascii)
  (:import-from #:tui/terminal
                #:terminal-output #:terminal-size #:flush-output #:terminal-invalid-p
                #:terminal-cursor-visible-p #:terminal-cursor-shape #:terminal-renderer
                #:synchronized-output-supported-p)
  (:export #:frame
           #:frame-buffer
           #:frame-area
           #:frame-count
           #:set-cursor
           #:hide-cursor
           #:draw
           #:cursor-shapes))

(in-package #:tui/render)

;;; Frames

(defstruct (frame (:constructor make-frame ()) (:copier nil))
  "What the application renders into during DRAW: the buffer for the whole screen and the
cursor to show when the frame is done."
  (buffer nil :type (or null buffer))
  (area nil)
  (count 0 :type fixnum)
  (cursor-x nil :type (or null fixnum))
  (cursor-y nil :type (or null fixnum))
  (cursor-shape :default :type keyword))

(defparameter *cursor-shapes*
  '((:default . 0) (:blinking-block . 1) (:block . 2) (:blinking-underline . 3)
    (:underline . 4) (:blinking-bar . 5) (:bar . 6))
  "Cursor shapes and their DECSCUSR codes.")

(defun cursor-shapes ()
  "The cursor shapes SET-CURSOR accepts."
  (mapcar #'car *cursor-shapes*))

(defun set-cursor (frame x y &optional (shape :default))
  "Show the cursor at column X, row Y with SHAPE (see CURSOR-SHAPES) when FRAME is drawn.
Without a call to SET-CURSOR during DRAW, the cursor is hidden."
  (assert (assoc shape *cursor-shapes*) (shape) "~S is not a cursor shape." shape)
  (setf (frame-cursor-x frame) x
        (frame-cursor-y frame) y
        (frame-cursor-shape frame) shape))

(defun hide-cursor (frame)
  "Hide the cursor when FRAME is drawn (the default)."
  (setf (frame-cursor-x frame) nil
        (frame-cursor-y frame) nil))

;;; Renderer state, one per terminal

(defstruct (renderer (:constructor make-renderer ()) (:copier nil))
  (current (make-buffer (make-rect 0 0 0 0)) :type buffer)
  (previous (make-buffer (make-rect 0 0 0 0)) :type buffer)
  (frame (make-frame) :type frame)
  (pen (make-pen) :type pen))

(defun terminal-renderer* (terminal)
  (or (terminal-renderer terminal)
      (setf (terminal-renderer terminal) (make-renderer))))

(defun fit-to-terminal (terminal renderer)
  "Resize RENDERER's buffers to TERMINAL's current size if it changed, which makes the next
frame a full redraw."
  (multiple-value-bind (columns rows) (terminal-size terminal)
    (let ((current (renderer-current renderer)))
      (unless (and (= columns (buffer-width current)) (= rows (buffer-height current)))
        (let ((area (make-rect 0 0 columns rows)))
          (resize-buffer current area)
          (resize-buffer (renderer-previous renderer) area)
          (setf (frame-area (renderer-frame renderer)) area
                (terminal-invalid-p terminal) t))))))

;;; Writing a frame

(defun write-mode (output mode set)
  (declare (type output output) (type fixnum mode))
  (output-csi output)
  (output-byte output 63)                       ; ?
  (output-decimal output mode)
  (output-byte output (if set 104 108)))        ; h or l

(defun write-cursor (output pen terminal frame)
  "Leave the cursor where FRAME asks for it, in its shape, or hidden."
  (let ((x (frame-cursor-x frame))
        (y (frame-cursor-y frame)))
    (cond ((and x y)
           (move-cursor output pen x y)
           (let ((shape (frame-cursor-shape frame)))
             (unless (eq shape (terminal-cursor-shape terminal))
               (output-csi output)
               (output-decimal output (cdr (assoc shape *cursor-shapes*)))
               (output-byte output 32)
               (output-byte output 113)         ; SP q
               (setf (terminal-cursor-shape terminal) shape)))
           (unless (terminal-cursor-visible-p terminal)
             (write-mode output 25 t)
             (setf (terminal-cursor-visible-p terminal) t)))
          ((terminal-cursor-visible-p terminal)
           (write-mode output 25 nil)
           (setf (terminal-cursor-visible-p terminal) nil)))))

(defun write-frame (terminal renderer)
  "Append the output that brings the screen from the previous frame to the current one."
  (let* ((output (terminal-output terminal))
         (pen (renderer-pen renderer))
         (sync (synchronized-output-supported-p terminal))
         (start (output-fill output)))
    (when sync (write-mode output 2026 t))
    (let ((body (output-fill output)))
      ;; The previous frame left default attributes and the cursor where PEN says. A full
      ;; redraw forgets the cursor: after a resize or RESUME it may be anywhere.
      (when (terminal-invalid-p terminal)
        (reset-pen pen)
        ;; Erase with default attributes, then draw everything that isn't blank.
        (reset-buffer (renderer-previous renderer))
        (output-ascii output (load-time-value (substitute (code-char 27) #\~ "~[0m~[2J") t))
        (setf (terminal-invalid-p terminal) nil))
      (write-diff output pen (renderer-previous renderer) (renderer-current renderer))
      (write-sgr-reset output pen)
      (write-cursor output pen terminal (renderer-frame renderer))
      (cond ((= body (output-fill output))
             ;; Nothing changed: send nothing at all.
             (setf (output-fill output) start))
            (sync (write-mode output 2026 nil))))))

(defun draw (terminal function)
  "Draw a frame on TERMINAL: call FUNCTION with a FRAME whose buffer covers the whole
screen and starts empty, then write to the terminal only the cells that differ from the
previous frame, and set the cursor. A change of the terminal's size or INVALIDATE makes the
frame redraw everything. When the terminal supports synchronized output the frame is
wrapped in it. The output, together with anything already in TERMINAL's output buffer, is
written with a single write(2). Return the frame."
  (let* ((renderer (terminal-renderer* terminal))
         (frame (renderer-frame renderer)))
    (fit-to-terminal terminal renderer)
    (reset-buffer (renderer-current renderer))
    (setf (frame-buffer frame) (renderer-current renderer)
          (frame-cursor-x frame) nil
          (frame-cursor-y frame) nil
          (frame-cursor-shape frame) :default)
    (funcall function frame)
    (write-frame terminal renderer)
    (flush-output terminal)
    (rotatef (renderer-current renderer) (renderer-previous renderer))
    (incf (frame-count frame))
    frame))
