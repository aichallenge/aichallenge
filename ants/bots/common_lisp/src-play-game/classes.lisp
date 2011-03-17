;;;; classes.lisp

(in-package :play-game)


;;; Classes

(defclass play-game-state (state)
  ((log-stream :reader log-stream :initform *debug-io*)
   (bots :reader bots :initarg :bots :initform nil)
   (map-file :reader map-file :initform nil)
   (orders :accessor orders :initarg :orders :initform nil)
   (players :reader players :initarg :players :initform nil)
   (procs :reader procs :initarg :procs :initform nil)))
