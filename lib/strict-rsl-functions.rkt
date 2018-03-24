#lang racket


(require (for-syntax syntax/parse))

(begin-for-syntax
  (struct rsl-func (args body)))

(define-syntax rs-x (rsl-func #'(x) #'(+ x 1)))

; (define-rsl-func (name arg) body)
(define-syntax (define-rsl stx)
  (syntax-parse stx
    [(_ (name arg) body ...)
     #'(define-syntax name (rsl-func #'arg #'(body ...)))]))

(define-rsl (rs1 x)
  (define y (+ x 2))
  y)

(define-rsl (rs2 y)
  (define x (- y 1))
   x)

(define comp1 ((rsl-compose rs1 rs2) 2))


(define-syntax (rsl-compose stx)
  (syntax-parse stx
    [(_ f1:id f2:id)
     #:do [(define f1-data (syntax-local-value #'f1))
           (define f2-data (syntax-local-value #'f2))]
     #:with f1-args (rsl-func-args f1-data) ;or use define/syntax-parse
     #:with (f1-body ...) (rsl-func-body f1-data)
     #:with f2-args (rsl-func-args f2-data)
     #:with (f2-body ...) (rsl-func-body f2-data)
     #'(lambda (f2-args)
       (let ([f1-args (let () f2-body ...)])
         f1-body ...))]))

#;(λ (x)
  (let-values ([(y) (let ()
                      (define y (+ x 1))
                      (values y))])
    (values (sub1 y))))
#|
Examples:
(define f1 (λ (x) (+ x 1))
(define f2 (λ (y) (- y 1))


|#