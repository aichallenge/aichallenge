#lang racket

(require "ants.rkt")

(define DIRECTIONS '(N E S W))

(define (do-setup) (void))

(define (do-turn)
  (define (move-ant a)
    (define (try-direction dir)
      (and (passable? (destination a dir))
	   (issue-order a dir)))
    (ormap try-direction DIRECTIONS))
  (for-each move-ant (get-my-ants)))

(define (main)
  (let loop ([map-data '()])
    (match (read)
     ['ready
      (setup map-data)
      (do-setup)
      (finish-turn)
      (loop '())]
     ['go
      (update map-data)
      (do-turn)
      (finish-turn)
      (loop '())]
     [(? eof-object?) (void)]
     [x (loop (cons x map-data))]
     )))

(main)