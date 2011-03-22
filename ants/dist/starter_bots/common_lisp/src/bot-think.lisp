;;;; bot-think.lisp
;;;;
;;;; Starter bot as described in:
;;;;
;;;;     http://github.com/aichallenge/aichallenge/wiki/Ants-Strategy-Guide

(in-package :ants-bot)


;;; Functions

(defun bot-think ()
  (loop for ant in (reverse (my-ants *state*))
        for row = (elt ant 0)
        for col = (elt ant 1)
        do (cond ((not (water? row col :north)) (move-ant row col :north))
                 ((not (water? row col :east))  (move-ant row col :east))
                 ((not (water? row col :south)) (move-ant row col :south))
                 ((not (water? row col :west))  (move-ant row col :west)))))
