;;;; bot-think.lisp
;;;;
;;;; Starter bot as described in:
;;;;
;;;;     http://github.com/aichallenge/aichallenge/wiki/Ants-Strategy-Guide

(in-package :ants-bot)


;;; Functions

(defun bot-think ()
  (loop for ant in (my-ants *state*)
        for x = (elt ant 0)
        for y = (elt ant 1)
        do (cond ((not (water? x y :north)) (move-ant x y :north))
                 ((not (water? x y :east))  (move-ant x y :east))
                 ((not (water? x y :south)) (move-ant x y :south))
                 ((not (water? x y :west))  (move-ant x y :west)))))
