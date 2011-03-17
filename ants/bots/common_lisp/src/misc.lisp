;;;; misc.lisp

(in-package :ants-bot)


;;; Functions

(defun end-of-turn ()
  (format (output *state*) "~&go~%")
  (force-output (output *state*)))


(defun move-ant (row col direction)
  (if (not (member direction '(:north :east :south :west)))
      (errmsg "[move-ant] Illegal direction: " direction)
      (format (output *state*) "~&o ~D ~D ~A~%" row col
              (case direction
                (:north "N")
                (:east  "E")
                (:south "S")
                (:west  "W")))))


(defun turn-time-remaining ()
  (- (+ (turn-start-time *state*) (turn-time *state*))
     (wall-time)))


(defun turn-time-used ()
  (- (wall-time) (turn-start-time *state*)))
