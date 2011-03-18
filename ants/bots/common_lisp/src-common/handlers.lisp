;;;; handlers.lisp

(in-package :ants-common)


;;; Functions

(defun address-in-use (arg)
  (declare (ignore arg))
  (errmsg "~&Address already in use. Aborting...~%")
  (quit 98))


(defun connection-lost (arg)
  (declare (ignore arg))
  (logmsg "~&Connection lost. Aborting...~%")
  (quit 103))


(defun connection-refused (arg)
  (declare (ignore arg))
  (logmsg "~&Connection refused. Aborting...~%")
  (quit :unix-status 111))


(defun error-handler (&optional arg)
  (logmsg "~&" arg " Aborting...~%")
  (quit :unix-status 1))


(defun socket-error-handler (arg)
  (declare (ignore arg))
  (errmsg "~&Socket error (trying to use a port < 1024?). Aborting...~%")
  (quit :unix-status 1))


(defun user-interrupt (arg)
  (declare (ignore arg))
  (logmsg "~&User interrupt. Aborting...~%")
  (quit))
