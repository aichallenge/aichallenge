;;;; handlers.lisp

(in-package :ants-bot)


;;; Functions

(defun address-in-use (arg)
  (declare (ignore arg))
  (errmsg "~&Address already in use. Aborting...~%")
  (quit 98))


(defun error-handler (&optional arg)
  (logmsg "~&" arg " Aborting...~%")
  (quit 1))


(defun socket-error-handler (arg)
  (declare (ignore arg))
  (errmsg "~&Socket error (trying to use a port < 1024?). Aborting...~%")
  (quit 1))


(defun user-interrupt (arg)
  (declare (ignore arg))
  (logmsg "~&User interrupt. Aborting...~%")
  (quit))
