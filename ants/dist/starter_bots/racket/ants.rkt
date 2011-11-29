#lang racket 

(provide setup update finish-turn print-params print-map 
	 get-my-ants destination passable? issue-order time-remaining)

;; convert all input to lowercase
(read-case-sensitive #f)

;; A Posn is a (cons Num Num) representing row,col numbers

;; hashes are maps from Posns to numbers representing owner
;; food is [ListOf Posn]
(define-values (ants dead food hills) 
  (values (make-hash) (make-hash) empty (make-hash)))

;; setup variables
(define-values 
  (turn loadtime turntime rows cols turns viewr2 attackr2 spawnr2 p-seed)
  (values 0 0 0 0 0 0 0 0 0 0))

(define the-map empty) ;; 2d vector of map values

;; map values
(define-values (MY-ANT ANTS DEAD LAND FOOD WATER)
        (values 0      0    -1   -2   -3   -4))

(define (map-set! r c v)
  (vector-set! (vector-ref the-map r) c v))
(define (map-ref r c)
  (vector-ref (vector-ref the-map r) c))

;; start time for each turn is recorded in setup
(define START-TIME 0)


;; ----------------------------------------------------------------------------
;; debugging fns 
(define (print-params)
  (printf 
   (string-append 
    "turn = ~a\nloadtime = ~a\nturntime = ~a\nrows = ~a\ncols = ~a\n"
    "turns = ~a\nviewradius2 = ~a\nattackradius2 = ~a\nspawnradius2 = ~a\n"
    "player_seed = ~a\n")
   turn loadtime turntime rows cols turns viewr2 attackr2 spawnr2 p-seed))

(define (print-map-cell x)
  (printf "~a  "
	  (cond
	    [(= x LAND)  "."]
	    [(= x FOOD)  "F"]
	    [(= x WATER) "W"]
	    [(= x DEAD)  "D"]
	    [else x])))
(define (print-map)
  (printf "   ")
  (for ([j (in-range cols)]) 
       (if (< j 10)
	   (printf "~a  " j)
	   (printf "~a " j)))
  (printf "\n")
  (for ([i (in-range rows)])
    (if (< i 10)
	(printf " ~a " i)
	(printf "~a " i))
    (for ([j (in-range cols)])
	 (print-map-cell (map-ref i j)))
    (printf "\n")))


;; ----------------------------------------------------------------------------
;; functions that process received data

;; reads and stores setup parameters from stdin
;; initializes the-map to all LAND
(define (setup map-data)
  (match map-data
    [(list ps  'player_seed 
   	   sr2 'spawnradius2
	   ar2 'attackradius2
	   vr2 'viewradius2
	   ts  'turns
	   cs  'cols
	   rs  'rows
	   tt  'turntime
	   lt  'loadtime
	   t   'turn)
     (set!-values 
      (turn loadtime turntime rows cols turns viewr2 attackr2 spawnr2 p-seed)
      (values t lt   tt       rs   cs   ts    vr2    ar2      sr2     ps))
     (set! the-map (build-vector rows (lambda (x) (make-vector cols LAND))))
     ]))

;; reset ants,dead,food,hills from previous turn to LAND
;; updates the-map based on received data
(define (update map-data)
  (reset-ants-dead-food-hills)
  (set! START-TIME (current-seconds))
  (let loop ([data (reverse map-data)])
    (unless (empty? data)
      (match data
	[(list-rest 'turn t rst)
	 (set! turn t)
	 (loop rst)]
	[(list-rest 'w r c rst)
	 (map-set! r c WATER)
	 (loop rst)]
	[(list-rest 'f r c rst)
	 (map-set! r c FOOD)
	 (set! food (cons (cons r c) food))
	 (loop rst)]
	[(list-rest 'a r c o rst)
	 (map-set! r c o)
	 (hash-set! ants (cons r c) o)
	 (loop rst)]
	[(list-rest 'd r c o rst)
	 ;; don't mark map as DEAD unless it's LAND
	 (when (= (map-ref r c) LAND) (map-set! r c DEAD))
	 (hash-set! dead (cons r c) o)
	 (loop rst)]
	[(list-rest 'h r c o rst)
	 (hash-set! hills (cons r c) o)
	 (loop rst)]
	))))
       
(define (reset-ants-dead-food-hills)
  (for-each (match-lambda [(cons r c) (map-set! r c LAND)]) (hash-keys ants))
  (for-each (match-lambda [(cons r c) (map-set! r c LAND)]) (hash-keys dead))
  (for-each (match-lambda [(cons r c) (map-set! r c LAND)]) food)
  (set!-values (ants dead food hills) 
	       (values (make-hash) (make-hash) empty (make-hash))))

(define (finish-turn)
  (displayln 'go)
  (flush-output))


;; ----------------------------------------------------------------------------
;; functions used during a turn

;; returns list of Posns representing my ants (ie, owner = MY-ANT)
(define (get-my-ants)
  (map car
       (filter (match-lambda [(cons k v) (= v MY-ANT)])
	       (map cons (hash-keys ants) (hash-values ants)))))

;; given direction and current loc, calculate next loc
;; (same as ants.destination in ants.py)
(define (destination loc dir)
  (match loc
    [(cons r c) 
     (define-values (r-delt c-delt)
       (case dir
	 [(N) (values -1 0)]
	 [(E) (values  0 1)]
	 [(S) (values  1 0)]
	 [(W) (values  0 -1)]))
     (cons (modulo (+ r r-delt) rows)
	   (modulo (+ c c-delt) cols))]))

;; true if given loc is not WATER
(define (passable? loc)
  (match loc
    [(cons r c)
     (not (= (map-ref r c) WATER))]))

(define (issue-order loc dir)
  (match loc
    [(cons r c)
     (printf "o ~a ~a ~a\n" r c dir)
     (flush-output)]))

;; time remaining, in ms, for current turn
(define (time-remaining)
  (- turntime (* 1000 (- (current-seconds) START-TIME))))