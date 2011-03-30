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
        do (cond ((not (water? row col :north)) (issue-order row col :north))
                 ((not (water? row col :east))  (issue-order row col :east))
                 ((not (water? row col :south)) (issue-order row col :south))
                 ((not (water? row col :west))  (issue-order row col :west)))))
