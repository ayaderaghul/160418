#lang racket
(require plot)
(plot-new-window? #t)

(provide (all-defined-out))

(define (population-mean->lines data)
  (define coors
    (for/list ([d (in-list data)]
               [n (in-naturals)])
      (list n d)))
  (lines coors))

(define (mean->lines-i data from to)
  (define coors
    (for/list ([d (in-list data)]
               [n (in-range from to)])
      (list n d)))
  (lines coors))

(define (compound d r)
  (foldl (lambda (n a) (+ a (expt d n))) 1 (build-list (- r 1) add1)))

(define (plot-mean-p data delta rounds)
  (define low (* 2 (compound delta rounds)))
  (define medium (* 5 (compound delta rounds)))
  (define high (* 8 (compound delta rounds)))
  (define line1
    (function (lambda (x) low) #:color "blue"))
  (define line2
    (function (lambda (x) medium) #:color "green"))
  (define line3
    (function (lambda (x) high) #:color "red"))
  (plot (list line1 line2 line3
              (population-mean->lines data))
        #:y-min 0 #:y-max (+ 5 high) #:width 1200 #:height 800))

(define (plot-mean data delta rounds pic tit)
  (define low (* 2 (compound delta rounds)))
  (define medium (* 5 (compound delta rounds)))
  (define high (* 8 (compound delta rounds)))
  (define line1
    (function (lambda (x) low) #:color "blue"))
  (define line2
    (function (lambda (x) medium) #:color "green"))
  (define line3
    (function (lambda (x) high) #:color "red"))
  (plot (list line1 line2 line3
              (population-mean->lines data))
        #:y-min 0 #:y-max (+ 5 high) #:width 1200 #:height 800
        #:out-file pic #:title tit))


(define (plot-mean-i data from to delta rounds pic tit)
  (define low (* 2 (compound delta rounds)))
  (define medium (* 5 (compound delta rounds)))
  (define high (* 8 (compound delta rounds)))
  (define line1
    (function (lambda (x) low) #:color "blue"))
  (define line2
    (function (lambda (x) medium) #:color "green"))
  (define line3
    (function (lambda (x) high) #:color "red"))
  (plot (list line1 line2 line3
              (mean->lines-i data from to))
        #:y-min 0 #:y-max (+ 5 high) #:width 1200 #:height 800
        #:out-file pic #:title tit))