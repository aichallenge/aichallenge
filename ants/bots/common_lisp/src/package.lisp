;;;; package.lisp

(in-package :cl-user)

(defpackage :ants-bot
  (:use :cl :ants-common :usocket)
  (:import-from :cl-user :quit))
