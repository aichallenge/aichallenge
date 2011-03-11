;;;; package.lisp

(in-package :cl-user)

(defpackage :ants-common
  (:use :cl)
  (:export :current-date-time-string :mkstr :starts-with :wall-time))
