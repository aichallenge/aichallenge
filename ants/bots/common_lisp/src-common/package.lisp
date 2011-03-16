;;;; package.lisp

(in-package :cl-user)

(defpackage :ants-common
  (:use :cl)
  (:import-from :cl-user :quit)
  (:export :address-in-use :connection-lost :connection-refused
           :current-date-time-string :errmsg :error-handler :host2str :logmsg
           :mkstr :par-value :print-game-map :socket-error-handler :starts-with
           :user-interrupt :wall-time))
