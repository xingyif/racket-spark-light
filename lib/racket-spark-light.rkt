#lang racket
; Cherry, Alexander
; Xing, Yifan

;; ---------------------------------------------------------------------------------------------------
;; API
(provide
 ;; We want to provide the full racket langauge
 (except-out (all-from-out racket)
             lambda)
 (rename-out [rsl-lambda lambda])
 ds-map
 ds-reduce
 mk-datashell
 ds-collect)

;; ---------------------------------------------------------------------------------------------------
;; DEPENDENCY

(require (for-syntax syntax/parse
                     syntax/kerncase))
(require ;; graph
         racket/struct
         (prefix-in un: racket))

(module+ test (require rackunit))



;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION

;; -----------------------------------------------------------------------------
;; SYNTAX (compile time functions)

;; (rsl-lambda (arg ...) body body-more ...)
;; rsl-lambda will create a racket lambda and attach a syntax property that
;; can be used to determine if it is a tfunc or afunc suitable for RSL MapReduce.
(define-syntax rsl-lambda
  (syntax-parser
    ;; TFunc, add the syntax property to mark this function as a TFunc
    [(_ (arg1:id) e1:expr e2:expr ...)
     #:with func #'(lambda (arg1) e1 e2 ...)
     #:with tagged (syntax-property #'func 'rsl-func-type 'tfunc)
     #'tagged]
    ;; AFunc, add the syntax property to mark this function as an AFunc
    [(_ (arg1:id arg2:id) e1:expr e2:expr ...)
     #:with func #'(lambda (arg1 arg2) e1 e2 ...)
     (syntax-property #'func 'rsl-func-type 'afunc)]
    ;; Regular lambda, not compatible with RSL
    [(_ e ...)
     #'(lambda e ...)]))

;; ds-map: (ds-map TFunc Datashell)
(define-syntax ds-map
  (syntax-parser
    #:datum-literals (tfunc)
    [(_ f ds)
     #:with exp (local-expand #'f (syntax-local-context) (kernel-form-identifier-list))
     ;#:with tfunc (syntax-property #'exp 'rsl-func-type)
     ;; cons the new function to the old queue of functions
     #'(Datashell (Datashell-dataset ds) (compose1 f (Datashell-op ds)))]))

;; ds-reduce: (ds-reduce AFunc Expr Datashell)
(define-syntax ds-reduce
  (syntax-parser
    #:datum-literals (afunc)
    [(_ f acc ds)
     #:with exp (local-expand #'f (syntax-local-context) (kernel-form-identifier-list))
     #:with afunc (syntax-property #'exp 'rsl-func-type)
     ;; perform the queued transformations, then run the reduction
     #'(foldl f acc (ds))]))



;; -----------------------------------------------------------------------------
;; RUNTIME LIB

;; [Listof Any] -> Datashell
;; Create a Datashell from a given list
(define (mk-datashell data-list)
  (if (list? data-list)
      (Datashell data-list (lambda (any) any))
      (error 'mk-datashell "First arg must be a list")))

;; collect-ds: Datashell -> [Listof Any]
(define (ds-collect ds)
  ;; apply composed function to the stored list, then return the list
  (define mapped (map (Datashell-op ds) (Datashell-dataset ds)))
  mapped)

;; (Datashell [Listof Any] 
(struct Datashell (dataset op) #:transparent #:property prop:procedure ds-collect)
