;;;; package.lisp

(in-package :cl-user)

(defpackage :ants-bot
  (:use :cl :usocket)
  (:import-from :cl-user :quit))
