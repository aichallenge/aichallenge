;;;; game-state.lisp
;;;;
;;;; The map is represented as a 2 dimensional array of fixnums:
;;;;
;;;;       0  = land
;;;;       1  = water
;;;;       2  = food
;;;;     100+ = live ant
;;;;     200+ = dead ant
;;;;
;;;; This basically limits the number of players to 100 (unless
;;;; MOST-POSITIVE-FIXNUM is lower than 300) before we get undefined
;;;; behaviour.

(in-package :ants-bot)


;;; Functions

(defun par-value (string)
  (parse-integer (subseq string (position #\space string) (length string))))


(defun parse-ant (bot string)
  (let* ((split (split-state-string string))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2)))
         (owner (parse-integer (elt split 3))))
    (if (= owner 0)
        (push (list col row) (slot-value bot 'my-ants))
        (push (list col row owner) (slot-value bot 'enemy-ants)))
    (setf (aref (game-map bot) row col) (+ owner 100))))


(defun parse-dead (bot string)
  (let* ((split (split-state-string string))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2)))
         (owner (parse-integer (elt split 3))))
    (setf (aref (game-map bot) row col) (+ owner 200))))


(defun parse-end-game (bot)
  (reset-game-map (game-map bot))
  (loop for line = (read-line (input bot) nil)
        until (starts-with line "go")
        do (cond ((starts-with line "f ") (parse-food bot line))
                 ((starts-with line "w ") (parse-water bot line))
                 ((starts-with line "a ") (parse-ant bot line))
                 ((starts-with line "d ") (parse-dead bot line)))))


(defun parse-food (bot string)
  (let* ((split (split-state-string string))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2))))
    (push (list col row) (slot-value bot 'food))
    (setf (aref (game-map bot) row col) 2)))


(defun parse-game-parameters (bot)
  (loop for line = (read-line (input bot) nil)
        until (starts-with line "ready")
        do (cond ((starts-with line "attackradius2 ")
                  (setf (slot-value bot 'attack-radius2) (par-value line)))
                 ((starts-with line "cols ")
                  (setf (slot-value bot 'cols) (par-value line)))
                 ((starts-with line "loadtime ")
                  (setf (slot-value bot 'load-time)
                        (/ (par-value line) 1000.0)))
                 ((starts-with line "rows ")
                  (setf (slot-value bot 'rows) (par-value line)))
                 ((starts-with line "spawnradius2 ")
                  (setf (slot-value bot 'spawn-radius2) (par-value line)))
                 ((starts-with line "turns ")
                  (setf (slot-value bot 'turns) (par-value line)))
                 ((starts-with line "turntime ")
                  (setf (slot-value bot 'turn-time)
                        (/ (par-value line) 1000.0)))
                 ((starts-with line "viewradius2 ")
                  (setf (slot-value bot 'view-radius2) (par-value line)))))
  (setf (slot-value bot 'game-map)
        (make-array (list (rows bot) (cols bot)) :element-type 'fixnum
                    :initial-element 0)))


(defun parse-game-state (bot)
  (reset-some-state bot)
  (logmsg "~&Receiving game state...~%")
  (loop for line = (read-line (input bot) nil)
        until (> (length line) 0)
        finally (cond ((starts-with line "end")
                       (parse-end-game bot)
                       (return-from parse-game-state t))
                      ((starts-with line "turn 0")
                       (setf (slot-value bot 'turn) 0)
                       (parse-game-parameters bot)
                       (return-from parse-game-state nil))
                      ((starts-with line "turn ")
                       (setf (slot-value bot 'turn) (par-value line))
                       (parse-turn bot)
                       (return-from parse-game-state nil)))))


(defun parse-turn (bot)
  (reset-game-map (game-map bot))
  (loop for line = (read-line (input bot) nil)
        until (starts-with line "go")
        do (cond ((starts-with line "f ") (parse-food bot line))
                 ((starts-with line "w ") (parse-water bot line))
                 ((starts-with line "a ") (parse-ant bot line))
                 ((starts-with line "d ") (parse-dead bot line)))))


(defun parse-water (bot string)
  (let* ((split (split-state-string string))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2))))
    (setf (aref (game-map bot) row col) 1)))


(defun print-game-map (game-map &optional (stream *debug-io*))
  (loop with dim = (array-dimensions game-map)
        for y from 0 below (first dim)
        do (loop for x from 0 below (second dim)
                 for val = (aref game-map y x)
                 do (cond ((= val 0) (princ #\. stream))
                          ((= val 1) (princ #\% stream))
                          ((= val 2) (princ #\* stream))
                          ((>= val 200) (princ (code-char (- val 135)) stream))
                          ((>= val 100) (princ (code-char (- val 3)) stream))
                          (t (princ #\? stream))))
           (terpri stream)))


(defun reset-game-map (game-map)
  (loop with dim = (array-dimensions game-map)
        for y from 0 below (first dim)
        do (loop for x from 0 below (second dim)
                 when (> (aref game-map y x) 1)
                   do (setf (aref game-map y x) 0))))


(defun reset-some-state (bot)
  (setf (slot-value bot 'enemy-ants) nil
        (slot-value bot 'my-ants)    nil
        (slot-value bot 'food)       nil))


(defun split-state-string (string)
  (loop with result = nil
        with value = nil
        for c across string
        when (and (char= c #\space) value)
          do (push (coerce (nreverse value) 'string) result)
             (setf value nil)
        when (char/= c #\space)
          do (push c value)
        finally (when value
                  (push (coerce (nreverse value) 'string) result))
                (return (nreverse result))))
