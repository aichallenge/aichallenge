;;;; run-tests.lisp

;; lisp-unit generates some style warnings when being compiled
(setf *muffled-warnings* 'style-warning)


;;; Packages

(format t "~&Loading lisp-unit...~%")
(load "test/lisp-unit")

(format t "Loading ant-bot...~%")
(setf asdf:*central-registry* (list *default-pathname-defaults*))
(asdf:oos 'asdf:load-op :ants-bot)

(in-package :ants-bot)
(use-package :lisp-unit)

(format t "Loading test objects...~%")
(load "test/test-objects")


;;; Tests

(define-test end-of-turn
  (assert-equal (format nil "~&go~%") (end-of-turn)))

(define-test mkstr
  (assert-equal "abc1de23fG456HI" (mkstr "abc" 1 "de" 23 "f" 'g 456 'hi)))

(define-test par-value
  (assert-equal 1 (par-value "a 1"))
  (assert-equal 234 (par-value "bc 234"))
  ;; Ought to be an error but it isn't for now.  Added test so we know when
  ;; the implementation changes.
  (assert-equal 1 (par-value " 1"))
  (assert-error 'type-error (par-value "")))

#+sbcl (define-test par-value-sbcl
         (assert-error 'sb-int:simple-parse-error (par-value " "))
         (assert-error 'sb-int:simple-parse-error (par-value "a "))
         (assert-error 'sb-int:simple-parse-error (par-value "a 1.23"))
         (assert-error 'sb-int:simple-parse-error (par-value "a 1,23"))
         (assert-error 'sb-int:simple-parse-error (par-value "a b")))

(define-test split-state-string
  (assert-equal '("abc") (split-state-string "abc"))
  (assert-equal '("a" "bc") (split-state-string " a bc"))
  (assert-equal '("a" "b" "c") (split-state-string "a b c "))
  (assert-equal '("a") (split-state-string " a ")))

(define-test starts-with
  (assert-true (starts-with "abc" "a"))
  (assert-true (starts-with "abc" "ab"))
  (assert-true (starts-with "abc" "abc"))
  (assert-false (starts-with "abc" ""))
  (assert-false (starts-with "abc" "b"))
  (assert-false (starts-with "abc" "ac"))
  (assert-false (starts-with "abc" "abcd"))
  ;(assert-false (starts-with "" ""))  ; undefined
  (assert-false (starts-with "" "a"))
  (assert-false (starts-with "" "ab")))


;;; Run the tests.

(format t "--- running tests ---~%")
(run-tests)
(format t "~&")
(cl-user::quit)
