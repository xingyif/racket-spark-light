#lang s-exp "../lib/racket-spark-light.rkt"

(define x (mk-datashell '(12 2)))
(define xy (ds-map add1 x))
(define xz (ds-map (lambda (i) (+ 1 i)) x))

(define xy-res (ds-reduce (lambda (x y) (+ x y)) 0 xy))
(define xz-res (ds-reduce + 0 xz))

xy-res
xz-res

(ds-reduce (lambda (r acc) (+ r acc)) 0
           (ds-map (lambda (y) (* 2 y))
                   (ds-map (lambda (y) (* 2 y))
                           (mk-datashell '(1 3)))))