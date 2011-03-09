;;;; ants-bot.lisp
;;;;
;;;;  author: Erik Winkels (aerique@xs4all.nl)
;;;; created: 2011-03-01
;;;; license: Apache Software License
;;;;   notes: * tested on SBCL 1.0.45.debian

(in-package :ants-bot)


;;; Main Program

;; This MAIN is used on the competition server.
(defun main (&key (bot (make-instance 'bot)) (verbose nil))
  (setf *verbose* verbose)
  (logmsg "~&=== New Match: " (current-date-time-string) " ===~%")
  (handler-bind ((sb-sys:interactive-interrupt #'interrupted-by-user))
    (loop while (handler-case (peek-char nil (input bot) nil)
                  (sb-int:simple-stream-error nil))
          for move-start-time = (wall-time)
          for move-end-time = (wall-time :offset +max-turn-time+)
          for end-of-game-p = (parse-game-state bot)
          when end-of-game-p do (loop-finish)
          do (logmsg "~&[start] " (current-date-time-string) "~%")
             (bot-think bot)
             (end-of-turn (output bot))
             (let ((wall-time (wall-time)))
               (logmsg "~&[  end] move took " (- wall-time move-start-time)
                       " seconds (" (- move-end-time wall-time)
                       " left).~%")))))


;; This MAIN is called when you use the Makefile locally.
(defun main-for-local (&key (verbose t))
  (main :verbose verbose))


;; This MAIN is for the Slime REPL with bin/play-proxy-game.sh.
(defun main-for-proxybot (&key (verbose t))
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp))
        client input output stream)
    (handler-bind ((address-in-use-error #'address-in-use))
      (socket-bind socket #(127 0 0 1) 41807)
      (socket-listen socket 0)
      (format *debug-io* "Waiting for connection...~%")
      (force-output)
      (setf client (socket-accept socket)
            stream (socket-make-stream client :input t :output t
                                       :element-type 'character
                                       :buffering :line)
            input stream
            output stream)
      (format *debug-io* "Connected. Playing game...~%")
      (force-output))
    (unwind-protect
         (main :bot (make-instance 'bot :input input :output output)
               :verbose verbose)
      (socket-close client)
      (socket-close socket)
      (format *debug-io* "Game finished. Connection closed...~%"))))
