;;;; bot-think.lisp
;;;;
;;;; Starter bot as described in:
;;;;
;;;;     http://github.com/aichallenge/aichallenge/wiki/Ants-Strategy-Guide

(in-package :ants-bot)


;;; Functions

(defun bot-think (bot)
  (loop for ant in (my-ants bot)
        for x = (elt ant 0)
        for y = (elt ant 1)
        do (cond ((not (waterp :north bot x y)) (move-ant bot x y :north))
                 ((not (waterp :east  bot x y)) (move-ant bot x y :east))
                 ((not (waterp :south bot x y)) (move-ant bot x y :south))
                 ((not (waterp :west  bot x y)) (move-ant bot x y :west)))))
