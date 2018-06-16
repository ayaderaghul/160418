(require "inout.rkt")
(require "auto.rkt")
(require "char.rkt")

(define d (csvfile->list "/home/chi/Downloads/160418/199rank"))
(define da (population-at d 700 TOTAL))

