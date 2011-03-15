;;;; handlers.lisp

(in-package :ants-bot)


;;; Functions

(defun address-in-use (arg)
  (declare (ignore arg))
  (errmsg "~&Address already in use. Aborting...~%")
  (quit :unix-status 98))


(defun user-interrupt (arg)
  (declare (ignore arg))
  (logmsg "~&User interrupt. Aborting...~%")
  (quit))


(defun socket-error-handler (arg)
  (declare (ignore arg))
  (errmsg "~&Socket error (trying to use a port < 1024?). Aborting...~%")
  (quit))
