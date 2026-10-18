;;;; style.lisp — tests for colors, modifiers and styles.

(defpackage #:tui/tests/style
  (:use #:cl)
  (:import-from #:tui/tests/harness #:deftest #:is #:signals)
  (:import-from #:tui/style
                #:rgb #:indexed #:rgb-color-p #:color-red #:color-green #:color-blue
                #:color-designator #:invalid-color #:+reset+ #:+red+ #:+light-cyan+
                #:modifiers #:invalid-modifier #:+bold+ #:+italic+ #:+dim+ #:+underline+
                #:+curly-underline+ #:+double-underline+ #:+any-underline+ #:underline-style
                #:make-style #:style-fg #:style-bg #:style-underline-color
                #:style-add-modifiers #:style-sub-modifiers #:patch-style #:apply-modifiers
                #:style=))

(in-package #:tui/tests/style)

(deftest rgb-colors-pack-into-fixnums
  (let ((c (rgb 1 2 3)))
    (is (typep c 'fixnum))
    (is (rgb-color-p c))
    (is (= 1 (color-red c)))
    (is (= 2 (color-green c)))
    (is (= 3 (color-blue c)))
    (is (not (rgb-color-p (indexed 200))))
    (is (not (rgb-color-p +reset+)))
    (is (/= (rgb 0 0 0) (indexed 0)))))

(deftest color-designators
  (is (= +red+ (color-designator :red)))
  (is (= +light-cyan+ (color-designator :light-cyan)))
  (is (= +reset+ (color-designator :reset)))
  (is (= (rgb #x12 #xab #xff) (color-designator "#12abFF")))
  (is (= 42 (color-designator 42)))
  (signals invalid-color (color-designator :chartreuse))
  (signals invalid-color (color-designator "#12345"))
  (signals invalid-color (color-designator "#12345g"))
  (signals invalid-color (color-designator 1.5)))

(deftest modifier-designators
  (is (= +bold+ (modifiers :bold)))
  (is (= (logior +bold+ +italic+) (modifiers '(:bold :italic))))
  (is (= 0 (modifiers '())))
  (is (= +dim+ (modifiers +dim+)))
  (signals invalid-modifier (modifiers :sparkly))
  (signals invalid-modifier (modifiers '(:bold :sparkly))))

(deftest underline-style-priority
  (is (eq :none (underline-style 0)))
  (is (eq :single (underline-style +underline+)))
  (is (eq :curly (underline-style (logior +underline+ +curly-underline+))))
  (is (eq :double (underline-style (logior +underline+ +double-underline+))))
  (is (eq :none (underline-style (logandc2 (logior +underline+ +curly-underline+)
                                           +any-underline+)))))

(deftest make-style-fields
  (let ((s (make-style :fg :red :bg "#000000" :underline-color 9
                       :modifiers '(:bold :italic) :remove-modifiers :dim)))
    (is (= +red+ (style-fg s)))
    (is (= (rgb 0 0 0) (style-bg s)))
    (is (= 9 (style-underline-color s)))
    (is (= (logior +bold+ +italic+) (style-add-modifiers s)))
    (is (= +dim+ (style-sub-modifiers s))))
  (let ((s (make-style)))
    (is (null (style-fg s)))
    (is (= 0 (style-add-modifiers s)))))

(deftest adding-and-removing-the-same-modifier-adds-it
  (let ((s (make-style :modifiers :bold :remove-modifiers '(:bold :dim))))
    (is (= +bold+ (style-add-modifiers s)))
    (is (= +dim+ (style-sub-modifiers s)))))

(deftest patch-style-like-ratatui
  (let* ((base (make-style :fg :red :bg :blue :modifiers '(:bold :italic)))
         (patch (make-style :fg :green :remove-modifiers :bold :modifiers :dim))
         (result (patch-style base patch)))
    (is (= 2 (style-fg result)))
    (is (= 4 (style-bg result)))
    (is (= (logior +italic+ +dim+) (style-add-modifiers result)))
    (is (= +bold+ (style-sub-modifiers result))))
  ;; A later add cancels an earlier remove.
  (let ((result (patch-style (make-style :remove-modifiers :bold) (make-style :modifiers :bold))))
    (is (= +bold+ (style-add-modifiers result)))
    (is (= 0 (style-sub-modifiers result))))
  (let ((s (make-style :fg :red)))
    (is (eq s (patch-style nil s)))
    (is (eq s (patch-style s nil)))
    (is (null (patch-style nil nil)))))

(deftest apply-modifiers-to-a-set
  (let ((s (make-style :modifiers :italic :remove-modifiers :bold)))
    (is (= (logior +italic+ +dim+) (apply-modifiers (logior +bold+ +dim+) s)))
    (is (= +bold+ (apply-modifiers +bold+ nil)))))

(deftest style-equality
  (is (style= (make-style :fg :red) (make-style :fg 1)))
  (is (style= nil (make-style)))
  (is (not (style= (make-style :fg :red) (make-style :fg :blue))))
  (is (not (style= (make-style :modifiers :bold) nil))))
