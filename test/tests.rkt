#lang s-exp "../lib/racket-spark-light.rkt"
(define x (lambda (z) 3))

((lambda (z) 3) 4)

(ds-reduce (lambda (r acc) (+ r acc)) 0
           (ds-map (lambda (y) (* 2 y))
                   (ds-map (lambda (y) (* 2 y))
                           (mk-datashell '(1 3)))))

(define xx (mk-datashell '(12 2)))
(xx)