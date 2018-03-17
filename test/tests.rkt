#lang s-exp "../lib/racket-spark-light.rkt"
(define x (lambda (z) 3))


((lambda (z) 3) 4)

(ds-map (lambda (r) r) (mk-datashell '(1 3)))
