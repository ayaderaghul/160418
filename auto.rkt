#lang racket
(require "cons.rkt")
(provide (all-defined-out))

;; AUTOMATON
(struct automaton (payoff initial action-plan) #:transparent)

(struct action (l m) #:transparent)

;; round the propensity to act
;; for the 3 x 3 game, there is too much outcome to mutate over.
(define (round1 n)
  (/ (round (* n 10)) 10))
;; for the 2 x 2 game
(define (round2 n)
  (/ (round (* n 100)) 100))

;; accommodate the flaw in floating numbers
(define (random-decimal prob)
  (define n (inexact->exact (round (* prob 100))))
  (if (zero? n)
      0
      (round1 (exact->inexact (/ (random n) 100)))))

(define (generate-random-action-vector)
  (define l (random))
  (define m (random-decimal (- 1 l)))
  (action (round1 l) (round1 m)))

(define (generate-random-action-plan)
  (define (generate-random-contingency)
    (for/list ([i (in-range 3)])
      (generate-random-action-vector)))
  (for/list ([i (in-range 3)])
    (generate-random-contingency)))

(define (make-random-automaton)
  (automaton 0
             (generate-random-action-vector)
             (generate-random-action-plan)))

;; classic automata
(define (L) (automaton 0 (action 1 0)
                     (list 
                      (list (action 1 0) (action 1 0) (action 1 0))
                      (list (action 1 0) (action 1 0) (action 1 0))
                      (list (action 1 0) (action 1 0) (action 1 0)))))

(define (M) (automaton 0 (action 0 1)
                     (list 
                      (list (action 0 1) (action 0 1) (action 0 1))
                      (list (action 0 1) (action 0 1) (action 0 1))
                      (list (action 0 1) (action 0 1) (action 0 1)))))
(define (H) (automaton 0 (action 0 0)
                     (list 
                      (list (action 0 0) (action 0 0) (action 0 0))
                      (list (action 0 0) (action 0 0) (action 0 0))
                      (list (action 0 0) (action 0 0) (action 0 0)))))

(define (A) (automaton 0 (action 0 1)
                     (list
                      (list (action 0 0) (action 0 1) (action 1 0))
                      (list (action 0 0) (action 0 1) (action 1 0))
                      (list (action 0 0) (action 0 1) (action 1 0)))))
;; flatten
(define (flatten-automaton auto)
  (match-define (automaton pay init plan) auto)
  (define (flatten-action act)
    (match-define (action l m) act)
    (list l m))
  (define (flatten-contingency cont)
    (map flatten-action cont))
  (define (flatten-plan p)
    (map flatten-contingency p))
  (flatten (list 0 (flatten-action init) (flatten-plan plan))))
;; resurrect

(define (decompose a-list n)
  (define l (length a-list))
  (define this-many (/ l n))
  (for/list ([i (in-range this-many)])
    (take (drop a-list (* i n)) n)))

(define (resurrect a-list) ;; without payoff
  (define (resurrect-h a)
    (apply action a))
  (define pairs
    (decompose a-list 2))
  (define actions
    (map resurrect-h pairs))
  (define init
    (first actions))
  (define plan
    (decompose (rest actions) 3))
  (automaton 0 init plan))
  

;; helper of round-auto and to aid making automaton
(define (round-action-scheme act)
  (match-define (action l m) act)
  (action (round1 l) (round1 m)))
(define (round-contingency contingency)
  (map round-action-scheme contingency))
(define (round-action-plan plan)
  (map round-contingency plan))

(define (round-auto auto)
  (match-define (automaton pay init plan) auto)
  (automaton
   0
   (round-action-scheme init)
   (round-action-plan plan)))

(define (reset auto)
  (match-define (automaton pay init plan) auto)
  (automaton 0 init plan))

(define PAYOFF-TABLE
  (list
   (list (cons 2 2) (cons 2 5) (cons 2 8))
   (list (cons 5 2) (cons 5 5) (cons 0 0))
   (list (cons 8 2) (cons 0 0) (cons 0 0))))

(define (payoff action1 action2)
  (list-ref
   (list-ref PAYOFF-TABLE action1)
   action2))


;; PAIR MATCH
;; helper of interact
;; this is to randomise the action C or D according to propensity
;; 0 is C, 1 is D
(define (randomise action-scheme)
  (match-define (action l m) action-scheme)
  (define r (random))
  (for/last
      ([s (in-naturals)]
       [p (in-list (list l m 1))]
       #:final (< r p)) s))

(define (what-next? action1 action2 plan)
  (list-ref (list-ref plan action1) action2))

;; return details
(define (interact-d au1 au2)
  (match-define (automaton pay1 init1 plan1) au1)
  (match-define (automaton pay2 init2 plan2) au2)
  (define (whats-next? action1 action2)
    (cons
     (what-next? action1 action2 plan1)
     (what-next? action2 action1 plan2)))
  (define-values (next1 next2 p1 p2 results)
    (for/fold (
               [current1 init1]
               [current2 init2]
               [payoff1 pay1]
               [payoff2 pay2]
               [round-results '()])
              ([_ (in-list DELTAS)])
      (match-define (list a1 a2)
                    (list (randomise current1) (randomise current2)))
      (match-define (cons n1 n2) (whats-next? a1 a2))
      (match-define (cons pa1 pa2) (payoff a1 a2))
      (values n1 n2
              (+ payoff1 (* pa1 _))
              (+ payoff2 (* pa2 _))
              (cons (cons pa1 pa2) round-results)
              )))
  (values (reverse results)
          (automaton p1 init1 plan1)
          (automaton p2 init2 plan2)))

;; return short version of details
(define (interact-ds au1 au2)
  (match-define (automaton pay1 init1 plan1) au1)
  (match-define (automaton pay2 init2 plan2) au2)
  (define (whats-next? action1 action2)
    (cons
     (what-next? action1 action2 plan1)
     (what-next? action2 action1 plan2)))
  (define-values (next1 next2 p1 p2 results)
    (for/fold (
               [current1 init1]
               [current2 init2]
               [payoff1 pay1]
               [payoff2 pay2]
               [round-results '()])
              ([_ (in-list DELTAS)])
      (match-define (list a1 a2)
                    (list (randomise current1) (randomise current2)))
      (match-define (cons n1 n2) (whats-next? a1 a2))
      (match-define (cons pa1 pa2) (payoff a1 a2))
      (values n1 n2
              (+ payoff1 (* pa1 _))
              (+ payoff2 (* pa2 _))
              (cons (cons pa1 pa2) round-results)
              )))
  (values (take (reverse results) 20)
          (automaton p1 init1 plan1)
          (automaton p2 init2 plan2)))

;; return no details of round results

(define (interact au1 au2)
  (match-define (automaton pay1 init1 plan1) au1)
  (match-define (automaton pay2 init2 plan2) au2)
  (define (whats-next? action1 action2)
    (cons
     (what-next? action1 action2 plan1)
     (what-next? action2 action1 plan2)))
  (define-values (next1 next2 p1 p2 results)
    (for/fold (
               [current1 init1]
               [current2 init2]
               [payoff1 pay1]
               [payoff2 pay2]
               [round-results '()])
              ([_ (in-list DELTAS)])
      (match-define (list a1 a2)
                    (list (randomise current1) (randomise current2)))
      (match-define (cons n1 n2) (whats-next? a1 a2))
      (match-define (cons pa1 pa2) (payoff a1 a2))
      (values n1 n2
              (+ payoff1 (* pa1 _))
              (+ payoff2 (* pa2 _))
              (cons (cons pa1 pa2) round-results)
              )))
  (values 
          (automaton p1 init1 plan1)
          (automaton p2 init2 plan2)))

;; return only result of payoffs
(define (interact-r au1 au2)
  (match-define (automaton pay1 init1 plan1) au1)
  (match-define (automaton pay2 init2 plan2) au2)
  (define (whats-next? action1 action2)
    (cons
     (what-next? action1 action2 plan1)
     (what-next? action2 action1 plan2)))
  (define-values (next1 next2 p1 p2 results)
    (for/fold (
               [current1 init1]
               [current2 init2]
               [payoff1 pay1]
               [payoff2 pay2]
               [round-results '()])
              ([_ (in-list DELTAS)])
      (match-define (list a1 a2)
                    (list (randomise current1) (randomise current2)))
      (match-define (cons n1 n2) (whats-next? a1 a2))
      (match-define (cons pa1 pa2) (payoff a1 a2))
      (values n1 n2
              (+ payoff1 (* pa1 _))
              (+ payoff2 (* pa2 _))
              (cons (cons pa1 pa2) round-results)
              )))
  (cons
   (round1 p1) (round1 p2)
   ;; (automaton p1 init1 plan1)
   ;; (automaton p2 init2 plan2)
   ))

;; MUTATION
(define (mutate-a action-scheme)
  (match-define (action l m) action-scheme)
  (define what-action? (random 2))
  (define total (+ l m))
  (define increase (random-decimal (round1 (- 1 total))))
  (define (mutate-helper act)
    (define r (random 2))
    (define decrease (random-decimal act))
    (if (zero? r)
        (round1 (+ act increase))
        (round1 (- act decrease)))) 
  (if (zero? what-action?)
      (action (mutate-helper l) m)
      (action l (mutate-helper m))))
      
(define (mutate-c posn contingency)
  (match-define (list l m h) contingency)
  (cond
   [(zero? posn) (list (mutate-a l) m h)]
   [(= posn 1) (list l (mutate-a m) h)]
   [(= posn 2) (list l m (mutate-a h))]))

(define (mutate auto)
  (match-define (automaton pay initial plan) auto)
  (match-define (list l- m- h-) plan)
  (define r (random 10))
  (cond
   [(zero? r) (automaton pay (mutate-a initial) plan)]
   
   [(<= r 3) (automaton pay initial (list (mutate-c (- r 1) l-) m- h-))]
   [(<= r 6) (automaton pay initial (list l- (mutate-c (- r 4) m-) h-))]
   [(<= r 9) (automaton pay initial (list l- m- (mutate-c (- r 7) h-)))]))



;;benchmark

(define BENCHMARKS (list (L) (M) (H) (A)))

(define (benchmark au)
  (cons (interact-r au au)
        (for/list ([i (in-list BENCHMARKS)])
          (interact-r au i))))

(define (interact-g au aus)
  (for/list ([i (in-list aus)])
    (interact-r au i)))

(define (create-matrix au)
  (define ls (cons au BENCHMARKS))
  (for/list ([i (in-list ls)])
    (interact-g i ls)))

(define (create-matrix-l ls)
  (for/list ([i (in-list ls)])
    (interact-g i ls)))

(define (reverse-matrix mat)
  (define l (length mat))
  (define (col x) (map (lambda (ls) (list-ref ls x)) mat))
  (for/list ([i (in-range l)])
    (col i)))

(define (create-cell pair m-r m-c)
  (match-define (cons p1 p2) pair)
  (if (= p2 m-r)
      (if (= p1 m-c)
          (format "*~a ~a*" p1 p2)
          (format " ~a ~a*" p1 p2))
      (if (= p1 m-c)
          (format "*~a ~a " p1 p2)
          (format " ~a ~a " p1 p2))))

(define (create-row pairs m-r m-c-s)
  (for/list ([i (in-list pairs)]
             [m-c (in-list m-c-s)])
    (~a (create-cell i m-r m-c) #:min-width 15 #:align 'center)))
  
(define (print-matrix mat)
  (define r (length mat))
  (define c (length (first mat)))
  (define ms 
    (for/list ([row (in-list mat)])
      (define ls (map cdr row))
      (define m (apply max ls))
      m))
  (for/list ([i (in-list mat)]
             [m-r (in-list ms)])
    (apply string-append (create-row i m-r ms))))

(define (interact-m au aus num)
  (define res
    (for/list ([i (in-list aus)]
               [j (in-list num)])
      (cons
       (* (car (interact-r au i)) j)
       (* (cdr (interact-r au i)) j))))
  (cons
   (round1 (/ (apply + (map car res)) 100))
   (round1 (/ (apply + (map cdr res)) 100))))

(define (interact-m-r aus num au)
  (define res (interact-m au aus num))
  (reverse-p res))

(define (interact-m-itself aus num)
  (define res-m-h
    (for/list ([i (in-list aus)]
               [j (in-list num)])
      (* j (car (interact-m i aus num)))))
  (define res-m
    (round1 (/ (apply + res-m-h) 100)))
  (cons res-m res-m))

(define (benchmark-m mix)
  (define aus (map car mix))
  (define num (map cdr mix))
  (list
   (interact-m-itself aus num)
   (interact-m-r aus num (L))
   (interact-m-r aus num (M))
   (interact-m-r aus num (H))
   (interact-m-r aus num (A))
;;   (interact-m-r aus num (T1))
;;   (interact-m-r aus num (T2))))
   ))

(define (reverse-p pair)
  (match-define (cons a b) pair)
  (cons b a))
  
(define (create-matrix-m mix)
  (define aus (map car mix))
  (define num (map cdr mix))
  (cons
   (benchmark-m mix)
   (for/list ([i (in-list BENCHMARKS)])
     (cons (interact-m i aus num)
           (interact-g i BENCHMARKS)))))
  
  
;; kit
(define (match-classics au)
  (match-define (cons a1 a2) (interact-r au au))
  (define itself
    (/ (+ a1 a2) 2))
  (define (with-au2 au2)
    (match-define (cons a1 a2) (interact-r au au2))
    a1)
  (define (to-au2 au2)
    (match-define (cons a1 a2) (interact-r au au2))
    a2)
  (define thigh (to-au2 (H)))
  (define tmedium (to-au2 (M)))
  (define wlow (with-au2 (L)))
  (define waccom (with-au2 (A)))
  (define taccom (to-au2 (A)))
  (list itself wlow tmedium thigh waccom taccom)) 
