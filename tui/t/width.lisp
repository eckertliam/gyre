;;;; width.lisp — tests for grapheme segmentation and display width.

(defpackage #:tui/tests/width
  (:use #:cl)
  (:import-from #:tui/tests/harness #:deftest #:is #:signals)
  (:import-from #:tui/width
                #:char-width #:grapheme-end #:grapheme-width #:uncertain-width-p
                #:string-width #:do-graphemes))

(in-package #:tui/tests/width)

(defun str (&rest parts)
  "Build a string from characters, code points and strings."
  (with-output-to-string (out)
    (dolist (part parts)
      (etypecase part
        (character (write-char part out))
        (integer (write-char (code-char part) out))
        (string (write-string part out))))))

(defun clusters (string)
  "The grapheme clusters of STRING as (substring width) lists."
  (let ((result '()))
    (do-graphemes ((start end width) string)
      (push (list (subseq string start end) width) result))
    (nreverse result)))

(deftest ascii-width
  (is (= 1 (char-width #\a)))
  (is (= 11 (string-width "hello world")))
  (is (equal '(("a" 1) ("b" 1)) (clusters "ab")))
  (is (= 0 (string-width ""))))

(deftest cjk-is-wide
  (is (= 2 (char-width (code-char #x65E5))))
  (is (= 6 (string-width (str #x65E5 #x672C #x8A9E))))   ; 日本語
  (is (= 4 (string-width (str #xD55C #xAE00))))          ; 한글
  (is (= 2 (char-width (code-char #xFF21)))))            ; fullwidth A

(deftest combining-marks-join-their-base
  (let ((s (str "e" #x301 "x")))
    (is (equal (list (list (str "e" #x301) 1) (list "x" 1)) (clusters s)))
    (is (= 2 (string-width s))))
  (is (= 1 (string-width (str "a" #x308 #x301 #x323))))
  (is (= 0 (char-width (code-char #x301)))))

(deftest zwj-sequences-are-one-wide-grapheme
  (let ((family (str #x1F468 #x200D #x1F469 #x200D #x1F467))
        (rainbow-flag (str #x1F3F3 #xFE0F #x200D #x1F308)))
    (is (= 1 (length (clusters family))))
    (is (= 2 (string-width family)))
    (is (= 1 (length (clusters rainbow-flag))))
    (is (= 2 (string-width rainbow-flag)))))

(deftest vs16-makes-a-narrow-emoji-wide
  (is (= 1 (string-width (str #x2764))))                 ; ❤ text presentation
  (is (= 2 (string-width (str #x2764 #xFE0F))))          ; ❤️
  (is (= 1 (length (clusters (str #x2764 #xFE0F)))))
  (is (= 2 (string-width (str "#" #xFE0F #x20E3))))      ; keycap #
  (is (uncertain-width-p (str #x2764 #xFE0F) 0 2))
  (is (not (uncertain-width-p (str #x1F600) 0 1)))
  (is (not (uncertain-width-p "a" 0 1)))
  ;; VS16 after a character without the Emoji property changes nothing.
  (is (= 1 (string-width (str "x" #xFE0F)))))

(deftest emoji-presentation-is-wide
  (is (= 2 (char-width (code-char #x1F600))))
  (is (= 2 (string-width (str #x1F44D #x1F3FD))))        ; thumbs up + skin tone
  (is (= 1 (length (clusters (str #x1F44D #x1F3FD))))))

(deftest flags-pair-regional-indicators
  (is (equal (list (list (str #x1F1FA #x1F1F8) 2)) (clusters (str #x1F1FA #x1F1F8))))
  (is (= 2 (length (clusters (str #x1F1FA #x1F1F8 #x1F1E6)))))
  ;; A lone regional indicator has Emoji_Presentation, so it is wide too.
  (is (= 4 (string-width (str #x1F1FA #x1F1F8 #x1F1E6)))))

(deftest control-characters-have-no-width
  (dolist (code '(0 7 9 10 13 27 #x7F #x85 #x9F))
    (is (= 0 (char-width (code-char code))))
    (is (= 0 (grapheme-width (str code) 0 1))))
  (is (= 2 (string-width (str "a" 9 "b"))))
  (is (equal (list (list (str 13 10) 0)) (clusters (str 13 10)))))

(deftest other-zero-width-characters
  (is (= 0 (char-width (code-char #x200B))))             ; zero width space
  (is (= 0 (char-width (code-char #x200D))))             ; ZWJ
  (is (= 0 (char-width (code-char #xFE0F))))
  (is (= 1 (char-width (code-char #xAD)))))              ; soft hyphen

(deftest hangul-jamo-and-halfwidth-sound-marks
  (is (equal (list (list (str #x1100 #x1161 #x11A8) 2)) (clusters (str #x1100 #x1161 #x11A8))))
  (is (equal (list (list (str #xFF76 #xFF9E) 2)) (clusters (str #xFF76 #xFF9E)))))

(deftest substring-bounds
  (is (= 3 (string-width "abcdef" :start 1 :end 4)))
  (is (= 4 (grapheme-end (str "ab" "e" #x301 "c") 2 5))))

;;; Segmentation agrees with SB-UNICODE:GRAPHEMES.

(defparameter *tricky-codes*
  #(#x61 #x62 #x20 13 10 9 #x7F #x301 #x308 #x200D #x200C #xFE0F #xFE0E #x1F468 #x1F469
    #x1F467 #x2764 #x1F3F3 #x1F308 #x1F1FA #x1F1F8 #x1F1E6 #x1100 #x1161 #x11A8 #xAC00
    #xAC01 #x0600 #x0903 #x0915 #x094D #x1F3FD #x1F44D #x4E00 #xFF76 #xFF9E #xAD #x200B
    #x0E33 #x00E9 #x20E3 #x2639)
  "Code points exercising every grapheme break rule.")

(defun random-tricky-string (state)
  (let ((length (1+ (random 12 state))))
    (coerce (loop repeat length
                  collect (code-char (aref *tricky-codes* (random (length *tricky-codes*) state))))
            'string)))

(deftest segmentation-matches-sb-unicode
  (let ((state (sb-ext:seed-random-state 1234))
        (mismatches '()))
    (dotimes (i 5000)
      (let ((s (random-tricky-string state)))
        (unless (equal (sb-unicode:graphemes s) (mapcar #'first (clusters s)))
          (push s mismatches))))
    (is (null mismatches))))

;;; Measuring doesn't allocate.

(defvar *sink* 0)

(deftest measuring-does-not-allocate
  (let ((strings (list "plain ascii text"
                       (str #x65E5 #x672C "x" #x1F600 "e" #x301 #x2764 #xFE0F)
                       (str #x1F468 #x200D #x1F469 #x1F1FA #x1F1F8))))
    (dolist (s strings)
      (setf *sink* (string-width s)))
    (let ((before (sb-ext:get-bytes-consed)))
      ;; Enough iterations that any allocation would fill a 64 KB allocation region.
      (dotimes (i 20000)
        (dolist (s strings)
          (setf *sink* (string-width s))))
      (is (= 0 (- (sb-ext:get-bytes-consed) before))))))

;;; The property cache agrees with SB-UNICODE.

(deftest cached-properties-match-sb-unicode
  (let ((mismatches '()))
    (loop for code from 0 below #x30000 by 7
          for char = (code-char code)
          unless (and (eq (tui/unicode:break-class char) (sb-unicode:grapheme-break-class char))
                      (eq (tui/unicode:extended-pictographic-p char)
                          (and (sb-unicode:proplist-p char :extended-pictographic) t))
                      (eq (tui/unicode:emoji-p char)
                          (and (sb-unicode:proplist-p char :emoji) t)))
            do (push code mismatches))
    (is (null mismatches))))
