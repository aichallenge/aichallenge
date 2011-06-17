;;;; MyBot.lisp
;;;;
;;;; A wrapper to compile mybot.lisp and ants.lisp into MyBot.

;; This seems to work for now.
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
;(declaim (sb-ext:muffle-conditions style-warning))  ; doesn't work
;(declaim (sb-ext:muffle-conditions warning))        ; idem
(setf sb-ext:*muffled-warnings* 'style-warning)

(load "main.lisp")
(save-lisp-and-die "MyBot" :toplevel #'main :executable t)
