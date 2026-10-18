;;;; input-long.lisp — input decoder tests on long input: concatenated table rows
;;;; and a long paste fed in random chunks, and what decoding allocates.

(defpackage #:tui/tests/input-long
  (:use #:cl)
  (:import-from #:tui/tests/harness #:deftest #:is)
  (:import-from #:tui/tests/input
                #:bytes #:key #:paste #:decode #:idle-rows #:row-input #:row-events
                #:no-difference #:first-difference #:at-most)
  (:import-from #:tui/input #:make-decoder #:feed-decoder))

(in-package #:tui/tests/input-long)

;;; Long input in random chunks

(defun random-splits (length random-state)
  "Split points cutting LENGTH bytes into random pieces of 1 to 7 bytes."
  (loop for split = (+ 1 (random 7 random-state)) then (+ split 1 (random 7 random-state))
        while (< split length)
        collect split))

(deftest concatenated-rows-in-random-chunks
  (let ((random-state (sb-ext:seed-random-state 1))
        (rows (coerce (idle-rows) 'vector)))
    (dotimes (round 20)
      (let* ((chosen (loop repeat 300
                           collect (aref rows (random (length rows) random-state))))
             (input (apply #'concatenate '(simple-array (unsigned-byte 8) (*))
                           (mapcar #'row-input chosen)))
             (expected (loop for row in chosen append (row-events row))))
        (is (no-difference round (first-difference
                                  expected
                                  (decode input :splits (random-splits (length input)
                                                                       random-state)))))))))

(defun random-paste-text (random-state length)
  "Text of at least LENGTH UTF-8 bytes, including ESC, [ and digits but never
the end of a paste."
  (let ((pieces (vector "a" "Z" " " "[" "2" "0" "1" "~a"
                        (string #\Newline) (string #\Return) (string (code-char 27))
                        (string (code-char 233)) (string (code-char #x4e2d))
                        (string (code-char #x1f600)))))
    (with-output-to-string (out)
      (loop with bytes = 0
            while (< bytes length)
            do (let ((piece (aref pieces (random (length pieces) random-state))))
                 (write-string piece out)
                 (incf bytes (length (sb-ext:string-to-octets piece :external-format :utf-8))))))))

(deftest long-paste-in-random-chunks
  (let* ((random-state (sb-ext:seed-random-state 2))
         (text (random-paste-text random-state 10240))
         (input (bytes :esc "[200~" text :esc "[201~" "x")))
    (is (equalp (list (paste text) (key #\x)) (decode input)))
    (dotimes (round 10)
      (is (no-difference round (first-difference
                                (list (paste text) (key #\x))
                                (decode input :splits (random-splits (length input)
                                                                     random-state))))))))

;;; Allocation and buffers

(defun repeated (count &rest parts)
  "The bytes of PARTS repeated COUNT times."
  (let* ((piece (apply #'bytes parts))
         (result (make-array (* count (length piece)) :element-type '(unsigned-byte 8))))
    (dotimes (i count result)
      (replace result piece :start1 (* i (length piece))))))

(defun bytes-consed-by (function)
  (let ((before (sb-ext:get-bytes-consed)))
    (funcall function)
    (- (sb-ext:get-bytes-consed) before)))

(deftest decoding-allocates-per-event-not-per-byte
  (let ((decoder (make-decoder))
        (keys (repeated 2000 :esc "[1;5A" :esc "[97:65;6u"))
        (content (repeated 1024 "a")))
    (feed-decoder decoder keys)
    ;; Each key event is a struct and a cons, whatever the length of its sequence.
    (is (at-most (* 4000 160) (bytes-consed-by (lambda () (feed-decoder decoder keys)))))
    (feed-decoder decoder (bytes :esc "[200~"))
    (dotimes (i 32) (feed-decoder decoder content))
    (feed-decoder decoder (bytes :esc "[201~" :esc "[200~"))
    ;; Paste content goes into a buffer that is reused once it has grown.
    (is (at-most 4096 (bytes-consed-by
                       (lambda () (dotimes (i 32) (feed-decoder decoder content))))))))

(deftest paste-buffer-is-reused-after-a-huge-paste
  (let ((decoder (make-decoder))
        (huge (make-string 200000 :initial-element #\h)))
    (is (equalp (list (paste huge))
                (feed-decoder decoder (bytes :esc "[200~" huge :esc "[201~"))))
    (is (equalp (list (paste "small") (key #\x))
                (feed-decoder decoder (bytes :esc "[200~small" :esc "[201~x"))))))
