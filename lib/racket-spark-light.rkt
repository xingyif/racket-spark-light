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
 tagged-lambda
 ds-map
 mk-datashell)

;; ---------------------------------------------------------------------------------------------------
;; DEPENDENCY

(require (for-syntax syntax/parse
                     syntax/kerncase))
(require ;;graph
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
    [(_ (arg1:id) e1:expr e2:expr ...)
     #:with func #'(lambda (arg1) e1 e2 ...)
     #:with tagged (syntax-property #'func 'rsl-func-type 'tfunc)
     #'tagged]
    [(_ (arg1:id arg2:id) e1:expr e2:expr ...)
     #:with func #'(lambda (arg1 arg2) e1 e2 ...)
     (syntax-property #'func 'rsl-func-type 'afunc)]
    [(_ e ...)
     (displayln "case 3")
     #'(lambda e ...)]))

(define-syntax (tagged-lambda stx)
  (syntax-parse stx
    [(_ e)
     (syntax-property #'e 'rsl-func-type 'tfunc)]))

(define-syntax ds-map
  (syntax-parser
    #:datum-literals (tfunc)
    [(_ f ds)
     #:with exp (local-expand #'f (syntax-local-context) (kernel-form-identifier-list))
     #:with tfunc (syntax-property #'exp 'rsl-func-type)
     ;; TODO do something here
     #'(Datashell (Datashell-dataset ds) (Datashell-ops ds) empty)]))

(define-syntax ds-reduce
  (syntax-parser
    #:datum-literals (afunc)
    [(_ f ds)
     #:with exp (local-expand #'f (syntax-local-context) (kernel-form-identifier-list))
     #:with afunc (syntax-property #'exp 'rsl-func-type)
     ;; TODO do something here
     #'(Datashell (Datashell-dataset ds) (Datashell-ops ds) empty)]))

;; -----------------------------------------------------------------------------
;; RUNTIME LIB

(struct Datashell (dataset ops rest) #:transparent)

;; [Listof Any] -> Datashell
;; Create a Datashell from a given list
(define (mk-datashell data-list)
  (if (list? data-list)
      (Datashell data-list empty empty)
      (error 'mk-datashell "First arg must be a list")))
