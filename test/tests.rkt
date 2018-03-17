#lang s-exp "../lib/racket-spark-light.rkt"

(define x (mk-datashell '(12 2)))
(define xy (ds-map (lambda (t) (+ 1 t)) x))
(define xy-res (ds-reduce (lambda (x y) (+ x y)) 0 xy))

(ds-reduce (lambda (r acc) (+ r acc)) 0
           (ds-map (lambda (y) (* 2 y))
                   (ds-map (lambda (y) (* 2 y))
                           (mk-datashell '(1 3)))))

(define xx (mk-datashell '(12 2)))
(xx)
xy-res
