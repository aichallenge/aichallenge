;;;; ants-bot.lisp

(in-package :ants-bot)


;;; Main Program

;; This MAIN is used on the competition server.
(defun main (&key (log nil) (state (make-instance 'state)) (verbose nil))
  (let ((*state* state)
        (*verbose* verbose))
    (cond ((and log *verbose*)
           (setf (slot-value *state* 'log-stream)
                 (open log :direction :output :if-exists :append
                           :if-does-not-exist :create)))
          (*verbose*
           (setf (slot-value *state* 'log-stream) *debug-io*)))
    (logmsg "~&=== New Match: " (current-date-time-string) " ===~%")
    (handler-bind ((sb-sys:interactive-interrupt #'user-interrupt))
      (loop while (handler-case (peek-char nil (input *state*) nil)
                    (sb-int:simple-stream-error nil))
            for end-of-game-p = (parse-game-state)
            when end-of-game-p do (loop-finish)
            do (logmsg "--- turn: " (turn *state*) " ---~%")
               (logmsg "~&[start] " (current-date-time-string) "~%")
               (bot-think)
               (finish-turn)
               (logmsg "~&[  end] move took " (turn-time-used) " seconds ("
                       (turn-time-remaining) " left).~%")))))


;; This MAIN is called when you compile with "make local-bot".
(defun main-for-local (&key (log "MyBot.log") (verbose t))
  (main :log log :verbose verbose))
