;;;; caps.lisp — detecting terminal features by asking the terminal.
;;;;
;;;; Detection sends queries for the kitty keyboard protocol (CSI ? u) and synchronized
;;;; output (DECRQM for mode 2026), then primary device attributes (CSI c). Every terminal
;;;; answers the last one, so its reply means no more answers are coming: a feature whose
;;;; query got no answer before it is unsupported. Nothing is inferred from TERM or any other
;;;; environment variable. The replies arrive as events from the application's input
;;;; decoder; results are stored in the terminal object.

(defpackage #:tui/caps
  (:use #:cl)
  (:import-from #:tui/input
                #:feed-decoder #:kitty-keyboard-reply #:mode-report #:mode-report-mode
                #:mode-report-value #:mode-report-private #:device-attributes-reply)
  (:import-from #:tui/output #:output-ascii)
  (:import-from #:tui/terminal
                #:terminal-output #:flush-output #:terminal-detection #:record-features
                #:with-raw-mode #:wait-for-input #:read-input)
  (:export #:start-feature-detection
           #:handle-feature-reply
           #:feature-detection-pending-p
           #:finish-feature-detection
           #:detect-features))

(in-package #:tui/caps)

(defstruct (detection (:constructor make-detection ()) (:copier nil))
  "What the replies received so far say."
  (kitty-keyboard nil :type boolean)
  (synchronized-output nil :type boolean))

(defparameter *queries*
  (substitute (code-char 27) #\~ "~[?u~[?2026$p~[c")
  "The kitty keyboard flags query, DECRQM for synchronized output, and primary device
attributes, which ends detection.")

(defun start-feature-detection (terminal)
  "Send TERMINAL the feature queries. Pass every event the application's decoder produces
from then on to HANDLE-FEATURE-REPLY until FEATURE-DETECTION-PENDING-P is false, or call
FINISH-FEATURE-DETECTION when the application's own deadline passes. The terminal should
be in raw mode so that the replies aren't echoed or held back by line editing."
  (setf (terminal-detection terminal) (make-detection))
  (output-ascii (terminal-output terminal) *queries*)
  (flush-output terminal))

(defun feature-detection-pending-p (terminal)
  "True between START-FEATURE-DETECTION and the end of detection on TERMINAL."
  (not (null (terminal-detection terminal))))

(defun finish-feature-detection (terminal)
  "End feature detection on TERMINAL and store what it found: features whose queries
weren't answered are unsupported. If TERMINAL is inside WITH-TERMINAL and the kitty
keyboard protocol was found, the sequence that enables it is left in the output buffer."
  (let ((detection (terminal-detection terminal)))
    (when detection
      (setf (terminal-detection terminal) nil)
      (record-features terminal
                       :kitty-keyboard (detection-kitty-keyboard detection)
                       :synchronized-output (detection-synchronized-output detection))))
  terminal)

(defun handle-feature-reply (terminal event)
  "If EVENT answers one of TERMINAL's feature queries, record it and return true; otherwise
return NIL, and EVENT belongs to the application. The device attributes reply ends
detection, as FINISH-FEATURE-DETECTION does."
  (let ((detection (terminal-detection terminal)))
    (when detection
      (typecase event
        (kitty-keyboard-reply
         (setf (detection-kitty-keyboard detection) t))
        (mode-report
         (when (and (mode-report-private event) (= (mode-report-mode event) 2026))
           ;; Set or reset means the terminal knows the mode, as Helix checks.
           (setf (detection-synchronized-output detection)
                 (and (member (mode-report-value event) '(1 2)) t))
           t))
        (device-attributes-reply
         (finish-feature-detection terminal)
         t)
        (t nil)))))

(defun detect-features (terminal decoder &key (timeout 0.5))
  "Detect TERMINAL's features, blocking until the terminal has answered or TIMEOUT seconds
have passed, and store the results in TERMINAL. Input is read from TERMINAL and decoded with
DECODER, the application's decoder for it; return the other events that arrived meanwhile,
in order. TERMINAL is put in raw mode for the duration if it isn't already. Applications
with their own event loop use START-FEATURE-DETECTION instead."
  (let ((octets (make-array 4096 :element-type '(unsigned-byte 8)))
        (deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second))))
        (others '()))
    (with-raw-mode (terminal)
      (start-feature-detection terminal)
      (loop while (feature-detection-pending-p terminal)
            for remaining = (/ (- deadline (get-internal-real-time))
                               internal-time-units-per-second)
            while (plusp remaining)
            when (wait-for-input terminal remaining)
              do (dolist (event (feed-decoder decoder octets :end (read-input terminal octets)))
                   (unless (handle-feature-reply terminal event)
                     (push event others))))
      (finish-feature-detection terminal)
      (flush-output terminal))
    (nreverse others)))
