;;;; compile-mybot.lisp

(load "MyBot.lisp")
(save-lisp-and-die "MyBot" :toplevel #'main :executable t)
