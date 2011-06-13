;;;; MyBot.lisp
;;;;
;;;; A wrapper to compile mybot.lisp and ants.lisp into MyBot.

(load "main.lisp")
(save-lisp-and-die "MyBot" :toplevel #'main :executable t)
