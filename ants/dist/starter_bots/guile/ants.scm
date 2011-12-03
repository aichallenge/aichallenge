;;  Copyright (C) 2011  
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Ants AI Challenge framework

(use-modules (oop goops) (ice-9 rdelim) (ice-9 regex))

(define-macro (unless cond . body)
  `(if (not ,cond) (begin ,@body)))

(define name-and-value-regexp "([a-z0-9_]+) ([0-9]+)")
(define players-regexp "players ([0-9]+)")
(define score-regexp "score ([0-9 ]+)")
(define turn-regexp "turn ([0-9]+)")
(define type-regexp "([wfhad]) ([0-9]+) ([0-9]+)([ 0-9]?)")

(define get-value-from-string
  (lambda (str regexp)
    (let ([m (string-match regexp str)])
      (string->number (match:substring m 1) 10)
      )))

(define get-value-from-input
  (lambda (port regexp . mode)
    (let* ([rd (get-valid-string-from-port port)]
	   [m (string-match regexp rd)]
	   )
      (cond
       ((null? mode)
	(string->number (match:substring m 1) 10))
       ((eq? (car mode) 'group)
	(match:substring m 1))
       (else
	(error get-value-from-input "invalid mode" mode)
	)))
    ))

(define-syntax-rule (push l x)
  (set! l (cons x l)))

(define get-valid-string-from-port
  (lambda (port)
    (let lp () 
      (let ([rd (string-trim-both (read-line port))])
	(if (string-null? rd)
	    (lp)
	    rd
	    )))))

(define-class <ant> ()
  (owner #:accessor ant:owner #:init-keyword #:owner)
  (square #:accessor ant:square #:init-keyword #:square)
  (alive #:accessor ant:alive #:init-value #t)
  (ai #:accessor ant:ai #:init-keyword #:ai)
  )

(define-method (ant:alive? (self <ant>))
  (ant:alive self))

(define-method (ant:dead? (self <ant>))
  (not (ant:alive self)))

(define-method (ant:mine? (self <ant>))
  (eq? (ant:owner self) 'mine))

(define-method (ant:enemy? (self <ant>))
  (eq? (ant:owner self) 'enemy))

(define-method (ant:row (self <ant>))
  (square:row (ant:square self)))

(define-method (ant:col (self <ant>))
  (square:col (ant:square self)))

(define-method (ant:order (self <ant>) (direction <symbol>))
  (ai:order (ant:ai self) self direction))

;; FIXME: square doesn't have to be a class, it's too heavy.
(define-class <square> ()
  (ant #:accessor square:ant #:init-keyword #:ant)
  (row #:accessor square:row #:init-keyword #:row)
  (col #:accessor square:col #:init-keyword #:col)
  (water #:accessor square:water #:init-keyword #:water)
  (food #:accessor square:food #:init-keyword #:food)
  (hill #:accessor square:hill #:init-keyword #:hill)
  (ai #:accessor square:ai #:init-keyword #:ai)
  )

(define-method (square:land? (self <square>))
  (not (square:water self)))

(define-method (square:water (self <square>))
  (square:water self))

(define-method (square:food? (self <square>))
  (square:food self))

(define-method (square:hill? (self <square>))
  (square:hill self))

(define-method (square:ant? (self <square>))
  (ant:alive? (square:ant self)))

(define-method (square:neighbor (self <square>) (direction <symbol>))
  (let ([ds (string->symbol 
	     (string-upcase (symbol->string direction)))]
	[row (square:row self)]
	[col (square:col self)]
	[ai (square:ai self)]
	)
    (call-with-values
	(lambda ()
	  (case ds
	    ((N) (ai:normalize ai (1- row) col))
	    ((E) (ai:normalize ai row (1+ col)))
	    ((S) (ai:normalize ai (1+ row) col))
	    ((W) (ai:normalize ai row (1- col)))
	    (else
	     (throw 'square-incorrect-direction)
	     )))
      (lambda (r c)
	(array-ref (ai:map ai) r c)))
    ))

(define-class <ai> ()
  (world-map #:accessor ai:map #:init-keyword #:map)
  (turn-number #:accessor ai:turn-number #:init-value 0)
  (loadtime #:accessor ai:loadtime #:init-keyword #:loadtime)
  (turntime #:accessor ai:turntime)
  (rows #:accessor ai:rows)
  (cols #:accessor ai:cols)
  (turns #:accessor ai:turns)
  (viewradius2 #:accessor ai:viewradius2)
  (attackradius2 #:accessor ai:attackradius2)
  (spawnradius2 #:accessor ai:spawnradius2)
  (player_seed #:accessor ai:seed)
  (viewradius #:accessor ai:viewradius)
  (attackradius #:accessor ai:attackradius)
  (spawnradius #:accessor ai:spawnradius)
  (players #:accessor ai:players)
  (score #:accessor ai:score)
  (my-ants #:accessor ai:my-ants #:init-value '())
  (enemy-ants #:accessor ai:enemy-ants #:init-value '())
  (did-setup #:accessor ai:did-setup #:init-value #f)
  (stdin #:accessor ai:stdin #:init-thunk current-input-port)
  (stdout #:accessor ai:stdout #:init-thunk current-output-port)
  )

(define-method (ai:setup (self <ai>) . func)
  (ai:read-intro self)
  (or (null? func) ((car func) self))
  (let* ([stdout (ai:stdout self)]
	 [row (ai:rows self)]
	 [col (ai:cols self)]
	 [m (make-array #f row col)]
	 )
    (format stdout "~a~%" "go")
    (force-output stdout)
    (array-index-map! m
		      (lambda (r c)
			(make <square> 
			  #:water #f #:food #f #:hill #f
			  #:ant '() #:row r #:col c #:ai self)))
    (set! (ai:map self) m)
    (set! (ai:did-setup self) #t)
    ))
  
(define-method (ai:run (self <ai>) . func)
  (if (not (ai:did-setup self))
      (ai:setup self))
  (let lp ()
    (unless (ai:read-turn self)
	    (let ([stdout (ai:stdout self)])
	      (or (null? func) ((car func) self))
	      (format stdout "~a~%" "go")
	      (force-output stdout)
	      (lp))
	)))

(define-method (ai:read-intro (self <ai>))
  (let* ([stdin (ai:stdin self)]
	 [rd (get-valid-string-from-port stdin)]
	 )
    (unless (string=? rd "turn 0")
	    (warn (format #f "unexpected: ~s" rd)))
    (let lp ()
      (let ([rd (get-valid-string-from-port stdin)])
	(unless (string=? rd "ready")
		(let* ([m (string-match name-and-value-regexp rd)]
		       [name (match:substring m 1)]
		       [val (string->number 
			     (match:substring m 2) 10)]
		       )
		  (slot-set! self (string->symbol name) val)
		  (lp)
		  ))))
	(set! (ai:viewradius self) (sqrt (ai:viewradius2 self)))
	(set! (ai:attackradius self) (sqrt (ai:attackradius2 self)))
	(set! (ai:spawnradius self) (sqrt (ai:spawnradius2 self)))
	))

	   
(define-method (ai:read-turn (self <ai>))
  (let* ([stdin (ai:stdin self)]
	 [rd (get-valid-string-from-port stdin)]
	 [over #f] ;; turn continue
	 )
    (if (string=? rd "end") ;; end turn, read player info/score
	(let* ([players (get-value-from-input stdin players-regexp)]
	       [score (get-value-from-input stdin score-regexp 'group)]
	       [a (if (not score) 
		      (error "score" score "players" players "rd" rd))] 
	       [score-list (string-split score #\space)]
	       )
	  (set! (ai:turn-number self) #f) ;; game over
	  (set! (ai:players self) players)
	  (set! (ai:score self) score-list) 
	  (set! over #t)) ;; turn end
	(let ([turn-number (get-value-from-string rd turn-regexp)])
	  (set! (ai:turn-number self) turn-number)))

    (array-for-each (lambda (square)
		      (set! (square:food square) #f)
		      (set! (square:ant square) '())
		      (set! (square:hill square) #f))
		    (ai:map self))

    (set! (ai:my-ants self) '())
    (set! (ai:enemy-ants self) '())

    (let lp ()
      (let ([rd (get-valid-string-from-port stdin)])
	(unless (string=? rd "go")
		(let* ([m (string-match type-regexp rd)]
		       [type (string->symbol (match:substring m 1))]
		       [row (string->number (match:substring m 2) 10)]
		       [col (string->number (match:substring m 3) 10)]
		       [owner (string->number (match:substring m 4) 10)]
		       [mm (ai:map self)]
		       )
		  (case type
		    ((w) (set! (square:water (array-ref mm row col)) #t))
		    ((f) (set! (square:food (array-ref mm row col)) #t))
		    ((h) (set! (square:hill (array-ref mm row col)) owner))
		    ((a) 
		     (let* ([square (array-ref mm row col)]
			    [ant (make <ant> #:owner owner #:square square 
				       #:alive #t #:ai self)]
			    )
		       (set! (square:ant square) ant)
		       (if (eq? owner 0)
			   (push (ai:my-ants self) ant)
			   (push (ai:enemy-ants self) ant))
		       ))
		    ((d)
		     (let* ([square (array-ref mm row col)]
			    [ant (make <ant> #:owner owner #:square square 
				       #:alive #f #:ai self)]
			    )
		       (set! (square:ant square) ant)))
		    ((r)
		     #t) ;; pass
		    (else
		     (warn (format #f "unexpected: ~a" rd)))))
		(lp))
	over ;; return over turn?
	))))

(define-method (ai:order (self <ai>) (ant <ant>) (direction <symbol>))
  (let* ([stdout (ai:stdout self)]
	 [row (ant:row ant)]
	 [col (ant:col ant)]
	 )
    (format stdout "o ~a ~a ~a~%" row col direction)
    (force-output stdout)
    ))
		       
(define-method (ai:order (self <ai>) (row <integer>) 
			 (col <integer>) (direction <symbol>))
  (let ([stdout (ai:stdout self)])
    (format stdout "o ~a ~a ~a~%" row col direction)
    (force-output stdout)
    ))
		      
(define-method (ai:normalize (self <ai>) (row <integer>) (col <integer>))
  (let ([rows (ai:rows self)]
	[cols (ai:cols self)]
	)
  (values (modulo row rows) (modulo col cols))))


	  
	  
