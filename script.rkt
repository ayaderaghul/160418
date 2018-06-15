(require "inout.rkt")
(require "auto.rkt")
(require "char.rkt")

(define d (csvfile->list "/home/chi/Downloads/160418-3/991rank"))
(define da (population-at d 88200 1000000))
(define dat (resurrect-a da))
(define dat2 (resurrect-n da))
(define a (first dat))

