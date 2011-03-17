;;;; proxy-bot.lisp
;;;;
;;;; TODO use STATE class

(in-package :ants-proxy-bot)


;;; Parameters

(defparameter *verbose* nil)

(defparameter *input* *standard-input*)
(defparameter *output* *standard-output*)

(defparameter *host* #-allegro #(127 0 0 1) #+allegro "localhost")
(defparameter *port* 41807)

(defparameter +version+ "0.1a")


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


(defun process-cmdline-options ()
  (cond ((or (getopt :short-name "?") (getopt :short-name "h"))
         (help)
         (quit))
        ((getopt :short-name "r")
         (format t "~&proxy-bot (Ant Wars) version ~A~%" +version+)
         (quit)))
  (do-cmdline-options (option name value source)
    (when (or (equal name "v") (equal name "verbose"))
      (setf *verbose* t))
    (when (or (equal name "p") (equal name "port"))
      (setf *port* (parse-integer value)))))


;;; Main Program

(defsynopsis ()
  (text :contents "Proxy bot for Ant Wars.
")
  (group (:header "Connection options:")
    (stropt :short-name "p" :long-name "port" :argument-name "PORT"
            :description "Port to connect to (default: 41807)."))
  (group (:header "Debug options:")
    (flag :short-name "v" :long-name "verbose"
      :description "Verbose logging to \"proxy-bot.log\"."))
  (group (:header "Immediate exit options:")
    (flag :short-name "?" :description "Print this help and exit.")
    (flag :short-name "h" :long-name "help"
          :description "Print this help and exit.")
    (flag :short-name "r" :long-name "release"
          :description "Print version/release number and exit.")))


(defun main ()
  (make-context)
  (process-cmdline-options)
  (open-log)
  (let (socket)
    (logmsg "~&~%=== ProxyBot started: " (current-date-time-string) " ===~%")
    (logmsg "Connecting to real bot at " (host2str *host*) ":" *port* "...~%")
    (unwind-protect
         (handler-bind (#+sbcl (sb-int:simple-stream-error #'connection-lost)
                        #+sbcl (sb-sys:interactive-interrupt #'user-interrupt)
                        (usocket:connection-refused-error #'connection-refused)
                        (error #'error-handler))
           (setf socket (socket-connect *host* *port*))
           (loop with end-of-game-p = nil
                 with stream = (socket-stream socket)
                 while (peek-char nil *input* nil)  ; run until we receive EOF
                 for turn from 0
                 do (logmsg "--- turn: " turn " ---~%")
                    (logmsg "Sending game state... ")
                    (loop for line = (read-line *input* nil)
                          until (or (starts-with line "go")
                                    (starts-with line "ready"))
                          do (when line
                               (when (starts-with line "end")
                                 (setf end-of-game-p t))
                               (write-line line stream)
                               (force-output stream))
                          finally (when line
                                    (write-line line stream)
                                    (force-output stream)))
                    (logmsg "Receiving bot response... ")
                    (loop for line = (read-line stream nil)
                          until (or (starts-with line "go")
                                    end-of-game-p)
                          do (when line
                               (write-line line *output*)
                               (force-output *output*))
                          finally (when line
                                    (write-line line *output*)
                                    (force-output *output*)))
                    (when end-of-game-p
                      (loop-finish))))
      (ignore-errors (socket-close socket))))
  (close-log))
