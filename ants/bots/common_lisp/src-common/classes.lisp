;;;; classes.lisp

(in-package :ants-common)


;;; Classes

;; TODO add defaults
(defclass state ()
  ((input :reader input :initarg :input :initform *standard-input*)
   (output :reader output :initarg :output :initform *standard-output*)
   (error-stream :reader error-stream :initarg :error-stream
                 :initform *error-output*)
   (log-stream :reader log-stream :initform nil)  ; TODO? *debug-io*
   (turn :reader turn :initform nil)
   (turn-start-time :reader turn-start-time :initform nil)
   (attack-radius2  :reader attack-radius2 :initform 6)
   (load-time :reader load-time :initform 3000)
   (spawn-radius2 :reader spawn-radius2 :initform 6)
   (turn-time :reader turn-time :initform 1000)
   (turns :reader turns :initform nil)
   (view-radius2 :reader view-radius2 :initform 93)
   (rows :reader rows :initform nil)
   (cols :reader cols :initform nil)
   (game-map :reader game-map :initform nil)
   ;; TODO move enemy-ants and my-ants to a subclass in :ants-bot
   (enemy-ants :reader enemy-ants :initform nil)
   (my-ants :reader my-ants :initform nil)
   (food :reader food :initform nil)))
