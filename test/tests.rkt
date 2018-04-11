#lang s-exp "../lib/racket-spark-light.rkt"

;; to make test macros
(require (for-syntax syntax/parse))
(require rackunit)

(define-rsl-func (add-3 num)
  (+ num 3))

(define-rsl-func (mult-2 num)
  (* num 2))

(define-datashell x (mk-datashell '(12 2)))
(define-datashell xy (ds-map add-3 x))
(define-datashell xyz (ds-map mult-2 xy))

(define-datashell xz (ds-map (lambda (i) (+ 3 i)) x))

(check-equal? (ds-collect x) '(12 2))
(check-equal? (ds-collect xy) '(15 5))
(check-equal? (ds-collect xy) (ds-collect xz))
(check-equal? (ds-collect xyz) '(30 10))


(define-rsl-func (print-1 num)
  (displayln 1)
  num)

(define-rsl-func (print-2 num)
  (displayln 2)
  num)

(define-datashell a (mk-datashell '(a b c d)))
(define-datashell ab (ds-map print-1 a))
(define-datashell abc (ds-map print-2 ab))

(ds-collect abc)

;; check that a basic reduce works
(define reversed (ds-reduce (lambda (curr acc) (cons curr acc)) '() abc))
(check-equal? reversed (reverse (ds-collect abc)))

;; we need to stop globally accessible variables maybe
(define shouldnt-work 5)
(define reversed-bad (ds-reduce (lambda (curr acc) (cons shouldnt-work acc)) '() abc))
reversed-bad

#|
;; only works if the underlying identifier is a datashell
(define-datashell xz-clone xz)

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
  (define-datashell (mk-datashell '(10 20))))

(define-syntax save-datashell
  (syntax-parser
    [(_ e ...)
     #'(define-datashell e ...)]))

;; Gives a static error, because save-ds can only be used at the top level
#;(define (y-mac)
  (define-datashell (mk-datashell '(10 20))))

;; Gives a dynamic error, must be a datashell, not a number
#;(define-datashell n 2)

(define-rsl-func (r1 x)
  (values (+ 1 x)))

(define-rsl-func (r2 y)
  (values y))

|#

#|



|#