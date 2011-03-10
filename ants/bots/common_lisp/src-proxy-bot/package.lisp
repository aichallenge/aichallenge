;;;; package.lisp

(in-package :cl-user)

(defpackage :ants-proxy-bot
  (:use :cl :usocket)
  (:import-from :cl-user :quit))
