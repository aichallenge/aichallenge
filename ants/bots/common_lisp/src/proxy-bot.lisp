;;;; proxy-bot.lisp
;;;;
;;;; TODO replace (equal line X) with (starts-with line X)
;;;;
;;;; Possible problem: once the proxy bot has send the end turn to the real
;;;; bot it doesn't send the bot response back to the game engine.

;;; Packages

(require :sb-bsd-sockets)  ; takes a second or so, so we need to be compiled

(defpackage :aw-proxy-bot
  (:use :cl :sb-bsd-sockets))

(in-package :aw-proxy-bot)


;;; Parameters

;(defparameter *verbose* t)
(defparameter *verbose* nil)  ; bot seems to work well, turning logging off

(defparameter *input* *standard-input*)
(defparameter *output* *standard-output*)


;;; Utility Functions

(let ((log nil))
  (defun close-log ()
    (when log
      (force-output log)
      (close log)))

  (defun logmsg (&rest args)
    (when log
      (format log (with-output-to-string (s) (dolist (a args) (princ a s))))
      (force-output log)))

  (defun open-log (&optional (file "proxy-bot.log"))
    (when *verbose*
      (setf log (open file :direction :output :if-exists :append
                           :if-does-not-exist :create)))))


(defun current-date-time-string ()
  (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
    (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month day hour min sec)))


;;; Handlers

(defun connection-refused (&optional arg)
  (declare (ignore arg))
  (logmsg "~&Connection refused. Aborting...~%")
  (cl-user::quit :unix-status 111))


(defun connection-reset (&optional arg)
  (declare (ignore arg))
  (logmsg "~&Connection reset by peer. Aborting...~%")
  (cl-user::quit :unix-status 104))


(defun error-handler (&optional arg)
  (logmsg "~&Error: " arg ".  Aborting...~%")
  (cl-user::quit :unix-status 1))


(defun interrupted-by-user (arg)
  (declare (ignore arg))
  (logmsg "~&Interrupted by user. Aborting...~%")
  (cl-user::quit))


;;; Main Program

(defun main ()
  (open-log)
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (logmsg "~&~%=== ProxyBot started: " (current-date-time-string) " ===~%")
    (logmsg "Connecting to real bot at 127.0.0.1:41807...~%")
    (handler-bind ((connection-refused-error #'connection-refused))
      (socket-connect socket #(127 0 0 1) 41807))
    (handler-bind ((sb-sys:interactive-interrupt #'interrupted-by-user)
                   (sb-int:simple-stream-error #'connection-reset)
                   (error #'error-handler))
      (loop with end-of-game-p = nil
            with stream = (socket-make-stream socket :input t :output t
                                              :element-type 'character
                                              :buffering :line)
            while (peek-char nil *input* nil)  ; run until we receive EOF
            for turn from 0
            do (logmsg "--- turn: " turn " ---~%")
               (logmsg "Sending game state... ")
               (loop with checksum = (list 0 0)
                     for line = (read-line *input* nil)
                     until (or (equal line "go") (equal line "ready"))
                     do (when line
                          (when (equal line "end")
                            (setf end-of-game-p t))
                          (incf (first checksum) (length line))
                          (incf (second checksum))
                          (write-line line stream)
                          (force-output stream))
                     finally (logmsg "checksum = " checksum ".~%")
                             (when line
                               (write-line line stream)
                               (force-output stream)))
               (logmsg "Receiving bot response... ")
               (loop with checksum = (list 0 0)
                     for line = (read-line stream nil nil)
                     until (or (equal line "go") end-of-game-p)
                     do (when line
                          (incf (first checksum) (length line))
                          (incf (second checksum))
                          (write-line line *output*)
                          (force-output *output*))
                     finally (logmsg "checksum = " checksum ".~%")
                             (when line
                               (write-line line *output*)
                               (force-output *output*)))
               (when end-of-game-p
                 (loop-finish)))
      (socket-close socket))
    (close-log)))
