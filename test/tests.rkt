#lang s-exp "../lib/racket-spark-light.rkt"

(require (for-syntax syntax/parse))
(require rackunit)

(save-ds x (mk-datashell '(12 2)))
(save-ds xy (ds-map add1 x))
(save-ds xz (ds-map (lambda (i) (+ 1 i)) x))

;; only works if the underlying identifier is a datashell
(save-ds xz-clone xz)

(define xy-res (ds-reduce (lambda (x y) (+ x y)) 0 xy))
(define xz-res (ds-reduce + 0 xz))

(check-equal? xy-res 16)
(check-equal? xz-res 16)

;; Ds-maps can be chained an arbitrary number of times
(check-equal? (ds-reduce (lambda (r acc) (+ r acc)) 0
                         (ds-map (lambda (y) (* 2 y))
                                 (ds-map (lambda (y) (* 2 y))
                                         (mk-datashell '(1 3)))))
              16)

;; Incorrect datashell argument
#;(ds-reduce (lambda (r) (+  r)) 0 2)
;; Incorrect function argument, must be TFunc
#;(ds-reduce (lambda (r) (+  r 1)) 0 (mk-datashell '(1 2 3)))

;; Gives a static error, because save-ds can only be used at the top level
#;(define (y)
  (save-ds (mk-datashell '(10 20))))

(define-syntax save-datashell
  (syntax-parser
    [(_ e ...)
     #'(save-ds e ...)]))

;; Gives a static error, because save-ds can only be used at the top level
#;(define (y-mac)
  (save-datashell (mk-datashell '(10 20))))

;; Gives a dynamic error, must be a datashell, not a number
#;(save-ds n 2)

(define-rsl (r1 x)
  (values (+ 1 x)))

(define-rsl (r2 y)
  (values y))

(compose-rsl r1 r2)

#|



|#