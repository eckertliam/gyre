;;;; cells.lisp — the storage behind a buffer: one specialized array per cell field, the
;;;; encoding of graphemes as integers and a side table for multi-code-point graphemes.
;;;;
;;;; This is the low-level layer shared by tui/buffer (which edits cells) and tui/render
;;;; (which diffs them). It is not part of the public API.

(defpackage #:tui/cells
  (:use #:cl)
  (:import-from #:tui/style #:+reset+)
  (:export #:buffer
           #:bufferp
           #:%make-buffer
           #:buffer-x
           #:buffer-y
           #:buffer-width
           #:buffer-height
           #:buffer-codes
           #:buffer-fgs
           #:buffer-bgs
           #:buffer-underline-colors
           #:buffer-modifier-sets
           #:buffer-flags
           #:buffer-clusters
           #:allocate-cells
           #:clear-cells
           #:+space+
           #:+continuation+
           #:+cluster-base+
           #:+wide+
           #:+uncertain+
           #:+skip+
           #:+always-update+
           #:+diff-option-flags+
           #:cluster-code-p
           #:cluster-string
           #:intern-cluster
           #:cluster-count
           #:clear-clusters
           #:same-grapheme-p
           #:same-cell-p))

(in-package #:tui/cells)

;;; Grapheme codes and flags

(defconstant +space+ 32 "The code of an empty cell.")
(defconstant +continuation+ #x110000
  "The code of the cell covered by the right half of a wide grapheme.")
(defconstant +cluster-base+ #x200000
  "Codes at or above this refer to entry (code - +cluster-base+) of the cluster table;
codes below it are single code points.")

(defconstant +wide+ 1 "Flag: the cell holds a grapheme two columns wide.")
(defconstant +uncertain+ 2 "Flag: terminals disagree on the grapheme's width (VS16).")
(defconstant +skip+ 4 "Flag: the diff never draws this cell.")
(defconstant +always-update+ 8 "Flag: the diff draws this cell even when unchanged.")
(defconstant +diff-option-flags+ 12)

(declaim (inline cluster-code-p))
(defun cluster-code-p (code)
  (declare (type (unsigned-byte 32) code))
  (>= code +cluster-base+))

;;; Cluster table: an open-addressing hash table from grapheme strings to indices.
;;; Looking up a grapheme that is already present doesn't allocate.

(defstruct (clusters (:constructor make-clusters ()) (:copier nil))
  (strings (make-array 8 :adjustable t :fill-pointer 0) :type (and vector (not simple-array)))
  (slots (make-array 32 :element-type 'fixnum :initial-element -1)
   :type (simple-array fixnum (*))))

(declaim (inline cluster-count))
(defun cluster-count (clusters)
  (fill-pointer (clusters-strings clusters)))

(defun cluster-hash (string start end)
  (declare (type simple-string string) (type fixnum start end) (optimize speed))
  (let ((hash 2166136261))
    (declare (type (unsigned-byte 32) hash))
    (loop for i of-type fixnum from start below end
          do (setf hash (logand (* (logxor hash (char-code (schar string i))) 16777619)
                                #xFFFFFFFF)))
    hash))

(defun clear-clusters (clusters)
  (setf (fill-pointer (clusters-strings clusters)) 0)
  (fill (clusters-slots clusters) -1)
  clusters)

(defun rehash (clusters)
  (let* ((strings (clusters-strings clusters))
         (slots (make-array (* 2 (length (clusters-slots clusters)))
                            :element-type 'fixnum :initial-element -1))
         (mask (1- (length slots))))
    (dotimes (index (fill-pointer strings))
      (let ((string (aref strings index)))
        (loop for slot = (logand (cluster-hash string 0 (length string)) mask)
                then (logand (1+ slot) mask)
              when (= (aref slots slot) -1)
                do (setf (aref slots slot) index)
                   (return))))
    (setf (clusters-slots clusters) slots)))

(defun intern-cluster (clusters string start end)
  "Return the index of the grapheme STRING[START,END) in CLUSTERS, adding it if needed."
  (declare (type simple-string string) (type fixnum start end))
  (let* ((slots (clusters-slots clusters))
         (strings (clusters-strings clusters))
         (mask (1- (length slots))))
    (loop for slot of-type fixnum = (logand (cluster-hash string start end) mask)
            then (logand (1+ slot) mask)
          for index of-type fixnum = (aref slots slot)
          do (cond ((= index -1)
                    (let ((new (vector-push-extend (subseq string start end) strings)))
                      (setf (aref slots slot) new)
                      (when (> (* 2 (fill-pointer strings)) (length slots))
                        (rehash clusters))
                      (return new)))
                   ((string= (aref strings index) string :start2 start :end2 end)
                    (return index))))))

;;; Buffers

(deftype codes () '(simple-array (unsigned-byte 32) (*)))
(deftype colors () '(simple-array (unsigned-byte 32) (*)))
(deftype modifier-sets () '(simple-array (unsigned-byte 16) (*)))
(deftype flags () '(simple-array (unsigned-byte 8) (*)))

(defstruct (buffer (:constructor %make-buffer (x y width height))
                   (:predicate bufferp)
                   (:copier nil))
  "A rectangular grid of cells at position (X, Y) of the screen. Cell (x, y) is element
(y - Y) * WIDTH + (x - X) of each array: its grapheme code, colors, modifier set and flags."
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (width 0 :type fixnum)
  (height 0 :type fixnum)
  (codes (make-array 0 :element-type '(unsigned-byte 32)) :type codes)
  (fgs (make-array 0 :element-type '(unsigned-byte 32)) :type colors)
  (bgs (make-array 0 :element-type '(unsigned-byte 32)) :type colors)
  (underline-colors (make-array 0 :element-type '(unsigned-byte 32)) :type colors)
  (modifier-sets (make-array 0 :element-type '(unsigned-byte 16)) :type modifier-sets)
  (flags (make-array 0 :element-type '(unsigned-byte 8)) :type flags)
  (clusters (make-clusters) :type clusters))

(defun clear-cells (buffer start end)
  "Make cells START to END of BUFFER empty: a space with default colors and no flags."
  (declare (type buffer buffer) (type fixnum start end))
  (fill (buffer-codes buffer) +space+ :start start :end end)
  (fill (buffer-fgs buffer) +reset+ :start start :end end)
  (fill (buffer-bgs buffer) +reset+ :start start :end end)
  (fill (buffer-underline-colors buffer) +reset+ :start start :end end)
  (fill (buffer-modifier-sets buffer) 0 :start start :end end)
  (fill (buffer-flags buffer) 0 :start start :end end)
  buffer)

(defun allocate-cells (buffer x y width height)
  "Give BUFFER position (X, Y) and size WIDTH × HEIGHT with all cells empty, reusing its
arrays when the cell count is unchanged."
  (declare (type buffer buffer) (type fixnum x y width height))
  (let ((count (* width height)))
    (unless (= count (length (buffer-codes buffer)))
      (flet ((cells (type) (make-array count :element-type type)))
        (setf (buffer-codes buffer) (cells '(unsigned-byte 32))
              (buffer-fgs buffer) (cells '(unsigned-byte 32))
              (buffer-bgs buffer) (cells '(unsigned-byte 32))
              (buffer-underline-colors buffer) (cells '(unsigned-byte 32))
              (buffer-modifier-sets buffer) (cells '(unsigned-byte 16))
              (buffer-flags buffer) (cells '(unsigned-byte 8)))))
    (setf (buffer-x buffer) x
          (buffer-y buffer) y
          (buffer-width buffer) width
          (buffer-height buffer) height)
    (clear-clusters (buffer-clusters buffer))
    (clear-cells buffer 0 count)))

(declaim (inline cluster-string))
(defun cluster-string (buffer code)
  "The grapheme string for the cluster code CODE of BUFFER."
  (declare (type buffer buffer) (type (unsigned-byte 32) code))
  (aref (clusters-strings (buffer-clusters buffer)) (- code +cluster-base+)))

(declaim (inline same-grapheme-p))
(defun same-grapheme-p (a code-a b code-b)
  "True when grapheme code CODE-A of buffer A and CODE-B of buffer B are the same grapheme."
  (declare (type (unsigned-byte 32) code-a code-b))
  (if (and (cluster-code-p code-a) (cluster-code-p code-b))
      (string= (cluster-string a code-a) (cluster-string b code-b))
      (= code-a code-b)))

(declaim (inline same-cell-p))
(defun same-cell-p (a i b j)
  "True when cell I of buffer A and cell J of buffer B look the same on screen."
  (declare (type buffer a b) (type fixnum i j))
  (and (same-grapheme-p a (aref (buffer-codes a) i) b (aref (buffer-codes b) j))
       (= (aref (buffer-fgs a) i) (aref (buffer-fgs b) j))
       (= (aref (buffer-bgs a) i) (aref (buffer-bgs b) j))
       (= (aref (buffer-underline-colors a) i) (aref (buffer-underline-colors b) j))
       (= (aref (buffer-modifier-sets a) i) (aref (buffer-modifier-sets b) j))))
