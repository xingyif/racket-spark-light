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
;; RSL lambda is currently regular lambda, may not be needed
(define-syntax rsl-lambda
  (syntax-parser
    ;; Regular lambda, not compatible with RSL
    [(_ e ...)
     #'(lambda e ...)]))

; ds-map: (ds-map TFunc Datashell)
; maps the given function on the Datashell and returns a new Datashell
; Transformation: lazily evaluated. Compose the given function with the previous functions
; but do not evaluated the given function
; Example: (ds-map (lambda (x) (+ 1 x)) (mk-datashell '(1 2)) -> (Datashell '2 3)
(define-syntax (ds-map stx)
  (syntax-parse stx
    #:literals (rsl-lambda)
    ;; Static checking for lambda literals
    [(_ (lambda (args ...) body ...) ds)
     #:with l (length (syntax->list #'(args ...)))
     #:fail-unless (= (eval #'l) 1) "lambda must have 1 argument in ds-map"
     #'(ds-map-func (lambda (args ...) body ...) ds)]
    [(_ f:id ds)
     #'(ds-map-func f ds)]))



; ds-reduce: AFunc Expr Datashell -> Any
; reduces the Datashell to a non Datashell type
; Action: eagerly evaluated, triggers all transformations stored on the datashell and the actor
; apply the final composed function on the given datashell and return the result
(define-syntax ds-reduce
  (syntax-parser
    #:literals (rsl-lambda)
    ;; Static checking for lambda literals
    [(_ (lambda (args ...) body ...) acc:expr ds)
     #:with l (length (syntax->list #'(args ...)))
     #:fail-unless (= (eval #'l) 2) "lambda must have 2 arguments in ds-reduce"
     #'(ds-reduce-func (lambda (args ...) body ...) acc ds)]
    [(_ f acc ds)
     ;; perform the queued transformations, then run the reduction
     #'(ds-reduce-func f acc ds)]))

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

;; ds-map-func: TFunc Datashell -> Datashell
;; Queues up a mapping to later be applied to a Datashell's data.
(define (ds-map-func tfunc ds)
  (if (procedure-arity-includes? tfunc 1)
      (Datashell (Datashell-dataset ds) (compose1 tfunc (Datashell-op ds)))
      (error 'ds-map "Invalid map function, must take 1 argument")))

;; ds-reduce-func: AFunc Any Datashell -> Any
;; Execute all queued mapping transformations onto the Datashell's data,
;; then reduces the data with the given function.
(define (ds-reduce-func afunc acc ds)
  (if (procedure-arity-includes? afunc 2)
      (foldl afunc acc (ds))
      (error 'ds-reduce "Invalid reduce function, must take 2 arguments")))
  
;; (Datashell [Listof Any] 
(struct Datashell (dataset op) #:transparent #:property prop:procedure ds-collect)
