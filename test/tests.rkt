#lang s-exp "../lib/racket-spark-light.rkt"

(define x (mk-datashell '(12 2)))
(define xy (ds-map (lambda (t) (+ 1 t)) x))
(define xy-res (ds-reduce (lambda (x y) (+ x y)) 0 xy))

xy-res