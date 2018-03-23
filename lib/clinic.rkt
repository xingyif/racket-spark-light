#lang racket

(require (for-syntax syntax/parse))

(begin-for-syntax
  (define x 1)
  (struct rsl-func (args body))
  )

(define-syntax (define-rsl stx)
  (syntax-parse stx
    [(_ (name arg) body ...)
    #'(define-syntax name (rsl-func #'arg #'(body ...)))]))
  
(define-rsl (rs1 x)
  (displayln x)
  (define y (+ 1 x))
   y)

(define-rsl (rs2 x)
  (displayln x)
  (define y (- x 2))
   y)

(define x ((rsl-compose rs1 rs2) 5))

#;(define-syntax rs1 (rsl-func #'x
                             #'((define y (+ 1 x)) y)))

(define-syntax (rsl-compose stx)
  (macro2 3)
  (syntax-parse stx
    [(_ f1:id f2:id)
     #:do [(define exp1 (syntax-local-value #'f1))
           (define exp2 (syntax-local-value #'f2))]
     #:with arg1 (rsl-func-args exp1)
     #:with (body1 ...) (rsl-func-body exp1)
     #:with arg2 (rsl-func-args exp2)
     #:with (body2 ...) (rsl-func-body exp2)
     (set! x 5)
     #'(lambda (arg2)
         (let ([arg1 (let () body2 ...)])
           body1 ...))]))
#|    
(lambda (x)
  (define y (+ x 1))
  (values y))

(lambda (y)
  (values (sub1 y)))

(lambda (x)
  (define y (+ x 1))
  (values (lambda (y-new) (values (sub1 y-new))) y))


(lambda (x)
  (let-values ([(y) (let ()
                      (define y (+ x 1))
                      (values y))])
    (values (sub1 y))))

|#
#|
(ds-map rs2 (ds-map rs1 ... l1))

(ds-reduce add-all ll)
|#