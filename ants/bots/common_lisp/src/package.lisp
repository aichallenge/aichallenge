;;;; package.lisp

(in-package :cl-user)

(defpackage :ants-bot
  (:use :cl :sb-bsd-sockets)
  (:import-from :cl-user :quit))
