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

(defun parse-game-parameters ()
  "Modifies *STATE*."
  (loop for line = (read-line (input *state*) nil)
        until (starts-with line "ready")
        do (cond ((starts-with line "attackradius2 ")
                  (setf (slot-value *state* 'attack-radius2) (par-value line)))
                 ((starts-with line "cols ")
                  (setf (slot-value *state* 'cols) (par-value line)))
                 ((starts-with line "loadtime ")
                  (setf (slot-value *state* 'load-time)
                        (/ (par-value line) 1000.0)))
                 ((starts-with line "rows ")
                  (setf (slot-value *state* 'rows) (par-value line)))
                 ((starts-with line "spawnradius2 ")
                  (setf (slot-value *state* 'spawn-radius2) (par-value line)))
                 ((starts-with line "turns ")
                  (setf (slot-value *state* 'turns) (par-value line)))
                 ((starts-with line "turntime ")
                  (setf (slot-value *state* 'turn-time)
                        (/ (par-value line) 1000.0)))
                 ((starts-with line "viewradius2 ")
                  (setf (slot-value *state* 'view-radius2) (par-value line)))))
  (setf (slot-value *state* 'game-map)
        (make-array (list (rows *state*) (cols *state*)) :element-type 'fixnum
                    :initial-element 0)))


(defun parse-game-state ()
  "Modifies *STATE*."
  (setf (slot-value *state* 'turn-start-time) (wall-time))
  (reset-some-state)
  (loop for line = (read-line (input *state*) nil)
        until (> (length line) 0)
        finally (return (cond ((starts-with line "end")
                               (parse-turn)
                               t)
                              ((starts-with line "turn 0")
                               (setf (slot-value *state* 'turn) 0)
                               (parse-game-parameters)
                               nil)
                              ((starts-with line "turn ")
                               (setf (slot-value *state* 'turn)
                                     (par-value line))
                               (parse-turn)
                               nil)))))


(defun parse-turn ()
  "Modifies *STATE* indirectly through RESET-GAME-MAP and PARSE-*."
  (reset-game-map)
  (loop for line = (read-line (input *state*) nil)
        until (starts-with line "go")
        do (cond ((starts-with line "f ") (set-food line))
                 ((starts-with line "w ") (set-water line))
                 ((starts-with line "a ") (set-ant line))
                 ((starts-with line "d ") (set-dead line)))))


(defun reset-game-map ()
  "Modifies *STATE*."
  (loop with game-map = (game-map *state*)
        with dim = (array-dimensions game-map)
        for row from 0 below (first dim)
        do (loop for col from 0 below (second dim)
                 when (> (aref game-map row col) 1)
                   do (setf (aref game-map row col) 0))))


(defun reset-some-state ()
  "Modifies *STATE*."
  (setf (slot-value *state* 'enemy-ants) nil
        (slot-value *state* 'my-ants)    nil
        (slot-value *state* 'food)       nil))


(defun set-ant (string)
  "Modifies *STATE*."
  (let* ((split (split-state-string string))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2)))
         (owner (parse-integer (elt split 3))))
    (if (= owner 0)
        (push (list row col) (slot-value *state* 'my-ants))
        (push (list row col owner) (slot-value *state* 'enemy-ants)))
    (setf (aref (game-map *state*) row col) (+ owner 100))))


(defun set-dead (string)
  "Modifies *STATE*."
  (let* ((split (split-state-string string))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2)))
         (owner (parse-integer (elt split 3))))
    (setf (aref (game-map *state*) row col) (+ owner 200))))


(defun set-food (string)
  "Modifies *STATE*."
  (let* ((split (split-state-string string))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2))))
    (push (list col row) (slot-value *state* 'food))
    (setf (aref (game-map *state*) row col) 2)))


(defun set-water (string)
  "Modifies *STATE*."
  (let* ((split (split-state-string string))
         (row (parse-integer (elt split 1)))
         (col (parse-integer (elt split 2))))
    (setf (aref (game-map *state*) row col) 1)))


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
