;;;; classes.lisp

(in-package :ants-bot)


;;; Classes

(defclass state ()
  ((input :reader input :initarg :input :initform *standard-input*)
   (output :reader output :initarg :output :initform *standard-output*)
   (log-stream :reader log-stream :initform nil)
   (attack-radius2 :reader attack-radius2 :initform nil)
   (load-time :reader load-time :initform nil)
   (spawn-radius2 :reader spawn-radius2 :initform nil)
   (turn :reader turn :initform nil)
   (turns :reader turns :initform nil)
   (turn-time :reader turn-time :initform nil)
   (view-radius2 :reader view-radius2 :initform nil)
   (cols :reader cols :initform nil)
   (rows :reader rows :initform nil)
   (game-map :reader game-map :initform nil)
   (enemy-ants :reader enemy-ants :initform nil)
   (my-ants :reader my-ants :initform nil)
   (food :reader food :initform nil)))
