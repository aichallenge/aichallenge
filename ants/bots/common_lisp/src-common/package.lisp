;;;; package.lisp

(in-package :cl-user)

(defpackage :ants-common
  (:use :cl)
  (:export :current-date-time-string :mkstr :par-value :print-game-map
           :starts-with :wall-time))
