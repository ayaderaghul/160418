#lang racket

(require plot)
(require "auto.rkt" "inout.rkt")
(provide (all-defined-out))

;; plot character

(define (gen-map xs type color label)
  (define result (match-classics type))
  (lines (map list xs result) 
         #:width 2
         #:color color 
         #:label label))

(define (gen-data-points xs type counter res)
    (if (zero? counter) 
        res
        (gen-data-points xs type (- counter 1)
                         (append (map list xs (match-classics type)) res))))

(define (gen-points color data)
  (points data
          #:color color
          #:fill-color color))

(define (plot-character au au-file)

  (define xs '(0 1 2 3 4 5))
  ;(define au-map (gen-map xs au 'orange "automaton"))
  (define au-data (gen-data-points xs au 100 '()))
  (define au-points (gen-points 'orange au-data))
  (define low-map (gen-map xs (L) 'brown "lows"))
  (define medium-map (gen-map xs (M) 'green "medium"))
  (define high-map (gen-map xs (H) 'red "high"))
  (define acc-map (gen-map xs (A) 'blue "accommodator"))

  (plot (list au-points ;low-map acc-map
              medium-map high-map)
        #:y-min -5 #:y-max 1000 
        #:x-min 0 #:x-max 7
        #:x-label "match with: 0. itself    1. wL    2. tM    3. tH    4. wA 5. tA"
        #:y-label "payoff"
        #:legend-anchor 'top-right #:width 600
        #:out-file au-file))

;;
(define (plot-characters cycles total-cycles id rank-file)
  (define data (csvfile->list rank-file))
  (define (plot-these cycles) 
    (for ([c (in-list cycles)])
      (define p (population-at data c total-cycles))
      (define a (first (resurrect-a p)))
      (define (gen-au-name)
        (string-append (number->string id) "-" (number->string c) ".png")) 
      (plot-character a (gen-au-name))))
  (plot-these cycles))
