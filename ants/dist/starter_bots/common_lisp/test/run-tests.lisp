;;;; run-tests.lisp

(require :asdf)

;; lisp-unit generates some style warnings when being compiled
(setf *muffled-warnings* 'style-warning)


;;; Packages

(format t "~&Loading lisp-unit...~%")
(load "test/lisp-unit")

(format t "Loading ant-bot...~%")
(load (merge-pathnames "3rd-party/asdf-init.lisp" *default-pathname-defaults*))
(asdf:oos 'asdf:load-op :ants-bot)

(in-package :ants-bot)
(use-package :lisp-unit)

(format t "Loading test objects...~%")
(load "test/test-objects")


;;; Tests

(define-test issue-order
  (assert-equal (format nil "~&o 1 1 N~%") (issue-order 1 1 :north))
  (assert-equal (format nil "~&o 2 2 E~%") (issue-order 2 2 :east))
  (assert-equal (format nil "~&o 2 3 S~%") (issue-order 2 3 :south))
  (assert-equal (format nil "~&o 3 2 W~%") (issue-order 3 2 :west)))

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


;;; Manipulating *STATE*

(setf (slot-value *state* 'input) (make-string-input-stream turn-0))
(parse-game-state)

(define-test game-parameters
  (assert-equal 6 (attack-radius2 *state*))
  (assert-equal 7 (cols *state*))
  (assert-equal 2.5 (load-time *state*))
  (assert-equal 4 (rows *state*))
  (assert-equal 6 (spawn-radius2 *state*))
  ;; This is somewhat counter-intuitive, but RUN-TESTS is run later and in the
  ;; meantime (TURN *STATE*) has been further modified.
  (assert-equal 1 (turn *state*))
  (assert-equal 500 (turns *state*))
  (assert-equal 2.0 (turn-time *state*))
  (assert-equal 93 (view-radius2 *state*)))

(setf (slot-value *state* 'input) (make-string-input-stream turn-1))
(parse-game-state)

(define-test distance
  (assert-equal 0.0 (distance 0 0 0 0))
  (assert-equal 1.0 (distance 0 0 1 0))
  (assert-equal 1.0 (distance 0 0 0 1))
  (assert-equal 1.4142135 (distance 0 0 1 1))
  (assert-equal 1.0 (distance 0 0 0 6))
  (assert-equal 1.0 (distance 0 0 3 0))
  (assert-equal 1.4142135 (distance 0 0 3 6))
  (assert-equal 3.0 (distance 0 0 0 3))
  (assert-equal 3.0 (distance 0 0 0 4))
  (assert-equal 2.236068 (distance 0 0 1 2))
  (assert-equal 3.6055512 (distance 0 0 2 3))
  (assert-equal 3.6055512 (distance 0 0 2 4))
  (assert-equal 2.236068 (distance 0 0 3 5)))

(define-test game-map-food
  (assert-equal '((1 2) (5 1)) (food *state*))
  (assert-equal 2 (aref (game-map *state*) 2 1))
  (assert-equal 2 (aref (game-map *state*) 1 5)))

(define-test game-map-land
  (assert-equal 0 (aref (game-map *state*) 1 0))
  (assert-equal 0 (aref (game-map *state*) 3 4)))

(define-test game-map-water
  (assert-equal 1 (aref (game-map *state*) 0 1))
  (assert-equal 1 (aref (game-map *state*) 1 3)))

(define-test water?
  (assert-true (water? 1 1 :north))
  (assert-true (water? 1 4 :west))
  (assert-true (water? 3 2 :south))
  (assert-true (water? 0 0 :east))
  (assert-false (water? 3 1 :north))
  (assert-false (water? 0 0 :west))
  (assert-false (water? 1 1 :south))
  (assert-false (water? 0 5 :east)))


;;; Run the tests.

(format t "--- running tests ---~%")
(run-tests)
(format t "~&")
(cl-user::quit)
