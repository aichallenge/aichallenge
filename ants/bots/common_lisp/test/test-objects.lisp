;;;; test-objects.lisp

(in-package :ants-bot)


;;; State

(setf *state* (make-instance 'state :output nil))


;;; Turns

;; In case you don't see them: there's some trailing spaces.
(defvar turn-0 "turn 0
loadtime 2500
turntime 2000 
rows  4
cols  7 
turns 500  
viewradius2   93
attackradius2   6  
spawnradius2 6
ready
")

;;   0 1 2 3 4 5 6
;; 0 . % % % % . .
;; 1 . . % % . * .
;; 2 . * % . . . .
;; 3 . . . . . . .
(defvar turn-1 "turn 1
f 1 5
f 2 1
w 0 1
w 0 2
w 0 3
w 0 4
w 1 2
w 1 3
w 2 2
go
")
