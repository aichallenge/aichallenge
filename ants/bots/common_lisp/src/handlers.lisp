;;;; handlers.lisp

(in-package :ants-bot)


;;; Functions

(defun address-in-use (arg)
  (errmsg "~&~S~%" arg)
  (quit))


(defun interrupted-by-user (arg)
  (declare (ignore arg))
  (logmsg "~&Interrupted by user... Aborting.~%")
  (quit))
