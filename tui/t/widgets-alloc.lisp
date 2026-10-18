;;;; widgets-alloc.lisp — rendering prebuilt widgets again into the same buffer allocates
;;;; nothing.

(defpackage #:tui/tests/widgets-alloc
  (:use #:cl)
  (:shadowing-import-from #:tui/widgets/block #:block)
  (:shadowing-import-from #:tui/widgets/list #:list)
  (:import-from #:tui/widgets/paragraph #:paragraph)
  (:import-from #:tui/widgets/list-state #:make-list-state #:list-select-next)
  (:import-from #:tui/widgets/table #:table)
  (:import-from #:tui/widgets/table-row #:table-row)
  (:import-from #:tui/widgets/table-state #:make-table-state #:table-select-next)
  (:import-from #:tui/widgets/tabs #:tabs)
  (:import-from #:tui/widgets/scrollbar #:scrollbar #:make-scrollbar-state)
  (:import-from #:tui/widgets/clear #:clear)
  (:import-from #:tui/tests/harness #:deftest #:is)
  (:import-from #:tui/tests/widgets #:str)
  (:import-from #:tui/layout #:make-rect #:make-layout #:split)
  (:import-from #:tui/style #:make-style)
  (:import-from #:tui/text #:span #:make-line #:make-text)
  (:import-from #:tui/buffer #:make-buffer #:reset-buffer)
  (:import-from #:tui/widgets/widget #:render #:render-stateful))

(in-package #:tui/tests/widgets-alloc)

(defvar *text*
  (str "The quick brown fox jumps over the lazy dog. " #x65E5 #x672C #x8A9E " and "
       #x1F600 " and e" #x301 " and " #x2764 #xFE0F ". "
       "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor."))

(defun consed-by (function iterations)
  "Bytes allocated by calling FUNCTION ITERATIONS times, after a few calls to warm up (the
first calls of a generic function with new classes of arguments update its dispatch, which
allocates)."
  (dotimes (i 10)
    (funcall function))
  (let ((before (sb-ext:get-bytes-consed)))
    (dotimes (i iterations)
      (funcall function))
    (- (sb-ext:get-bytes-consed) before)))

(defun widgets ()
  "One of each built-in widget, with styles, borders and titles."
  (let ((red (make-style :fg :red))
        (bold (make-style :modifiers :bold)))
    (values
     (block :borders :all :border-type :rounded :border-style red :style bold
            :titles (cl:list "left" (make-line "center" :alignment :center)
                             (cl:list :bottom (make-line "right" :alignment :right)))
            :title-style red :padding 1)
     (paragraph (make-text (cl:list (make-line (cl:list (span "styled " red) *text*))
                                    *text* (make-line *text* :alignment :center)))
                :block (block :borders :all :title "wrapped") :wrap t :style bold
                :scroll-y 1)
     (paragraph *text* :scroll-x 3 :alignment :left :block (block :borders '(:top)))
     (paragraph (make-text (cl:list *text* *text*) :alignment :right))
     (list (loop for i below 40 collect (format nil "Item ~D ~A" i *text*))
           :block (block :borders :all :title "list") :highlight-symbol ">> "
           :highlight-style red :scroll-padding 2 :repeat-highlight-symbol t)
     (table (loop for i below 40
                  collect (cl:list (format nil "~D" i) *text* (str #x65E5 #x672C)))
            :widths '((:length 3) (:fill 1) (:percentage 20))
            :header (table-row '("#" "Text" "CJK") :style bold :bottom-margin 1)
            :footer '("" "footer" "")
            :block (block :borders :all) :row-highlight-style red :highlight-symbol ">")
     (table (loop for i below 10 collect (cl:list "a" "b" "c")))
     (tabs (cl:list "One" (str #x65E5 #x672C) (span "Three" red) "Four" "Five")
           :block (block :borders :all) :selected 2)
     (scrollbar :style red)
     (clear))))

(defun render-all (buffer area areas)
  "Return a function that renders every widget into the areas of BUFFER, the stateful ones
with states that change every frame."
  (multiple-value-bind (block wrapped truncated aligned list table plain-table tabs bar clear)
      (widgets)
    (let ((list-state (make-list-state :selected 5))
          (table-state (make-table-state :selected 3))
          (bar-state (make-scrollbar-state :content-length 200 :position 50))
          (frame 0))
      (lambda ()
        (reset-buffer buffer)
        (render block (svref areas 0) buffer)
        (render wrapped (svref areas 1) buffer)
        (render truncated (svref areas 2) buffer)
        (render aligned (svref areas 3) buffer)
        (render-stateful list (svref areas 4) buffer list-state)
        (render list (svref areas 4) buffer)
        (render-stateful table (svref areas 5) buffer table-state)
        (render table (svref areas 5) buffer)
        (render plain-table (svref areas 6) buffer)
        (render tabs (svref areas 7) buffer)
        (render-stateful bar area buffer bar-state)
        (render clear (svref areas 8) buffer)
        (render *text* (svref areas 8) buffer)
        ;; Move the selections and the scrollbar around within the visible items.
        (setf frame (logand (1+ frame) 15))
        (when (zerop frame)
          (list-select-next list-state)
          (table-select-next table-state))))))

(deftest rendering-widgets-does-not-allocate
  (let* ((area (make-rect 0 0 120 60))
         (buffer (make-buffer area))
         (layout (make-layout :constraints '((:length 6) (:length 8) (:length 3) (:length 2)
                                             (:length 10) (:length 12) (:length 4) (:length 3)
                                             (:fill 1))))
         (areas (split layout area))
         (function (render-all buffer area areas)))
    ;; Each frame renders thousands of cells; any allocation in a frame would fill a 64 KB
    ;; allocation region well within these iterations.
    (is (= 0 (consed-by function 500)))))

(deftest rendering-each-widget-does-not-allocate
  (let* ((area (make-rect 0 0 60 20))
         (buffer (make-buffer area))
         (widgets (multiple-value-list (widgets))))
    (dolist (widget widgets)
      (let ((state (typecase widget
                     (list (make-list-state :selected 7))
                     (table (make-table-state :selected 7))
                     (scrollbar (make-scrollbar-state :content-length 30 :position 9)))))
        (is (= 0 (consed-by (if state
                                (lambda () (render-stateful widget area buffer state))
                                (lambda () (render widget area buffer)))
                            5000)))))))
