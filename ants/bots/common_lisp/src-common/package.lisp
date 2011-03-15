;;;; package.lisp

(in-package :cl-user)

(defpackage :ants-common
  (:use :cl)
  (:export :current-date-time-string :host2str :mkstr :par-value
           :print-game-map :starts-with :wall-time))
