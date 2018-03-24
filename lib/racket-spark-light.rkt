#lang racket
; Cherry, Alexander
; Xing, Yifan

;; ---------------------------------------------------------------------------------------------------
;; API
(provide
 ;; We want to provide the full racket langauge
 (except-out (all-from-out racket)
             lambda
             #%module-begin)
 (rename-out [rsl-lambda lambda]
             [rsl-module-begin #%module-begin])
 
 ;; ds-map: TFunc Datashell -> Datashell
 ;; Creates a new Datashell with the old Datashell mapped with the given function.
 ds-map
 
 ;; ds-reduce: AFunc Any Datashell -> Any
 ;; Collects the data in a Datashell, then reduces it with the given function and accumulator.
 ds-reduce
 
 ;; mk-datashell: [Listof Any] -> Datashell
 ;; Creates a Datashell data structure from a list of items.
 mk-datashell
 
 ;; ds-collect: Datashell -> [Listof Any]
 ;; Collects the data in a Datashell and returns it.
 ds-collect
 
 ;; save-ds: Id Datashell -> Void
 ;; EFFECTS: Binds the Datashell to the given identifier in the global scope. Must be used at the top-level.
 save-ds

 ;; (define-rsl (x x) Expr ... (values Expr ...))
 define-rsl

 compose-rsl
 create-rsl)

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

(define-syntax rsl-module-begin
  (syntax-parser
    [(_ e ...)
     #'(#%module-begin (validate-top-level e) ...)]))

;; rsl-lambda: Procedure -> Procedure
;; Example: (rsl-lambda (arg ...) body body-more ...)
;; RSL lambda is currently regular lambda, may not be needed
(define-syntax rsl-lambda
  (syntax-parser
    ;; Regular lambda, not compatible with RSL
    [(_ e ...)
     #'(lambda e ...)]))

;; validate-top-level: Syntax -> Syntax
;; Validates top-level save-ds's
(define-syntax validate-top-level
  (syntax-parser
    #:literals (save-ds)
    [(_ (save-ds e ...))
     #'(save-ds-top e ...)]
    [(_ e)
     (define f (local-expand #'e (syntax-local-context) #f))
     (syntax-parse f 
       #:literals (save-ds)
       [(save-ds args ...)
        #'(save-ds-top args ...)]
       [other
        #'other])]))

;; ds-map: TFunc Datashell -> Datashell
;; Maps the given function on the Datashell and returns a new Datashell
;; Transformation: lazily evaluated. Compose the given function with the previous functions
;; but do not evaluated the given function
;; Example: (ds-map (lambda (x) (+ 1 x)) (mk-datashell '(1 2)) -> (Datashell '2 3)
(define-syntax (ds-map stx)
  (syntax-parse stx
    #:literals (rsl-lambda)
    ;; Static checking for lambda literals
    [(_ (lambda (args ...) body ...) ds)
     #:with l (length (syntax->list #'(args ...)))
     #:fail-unless (= (eval #'l) 1) "lambda must have 1 argument in ds-map"
     ;; reconstruct lambda and pass to the runtime function
     #'(ds-map-func (lambda (args ...) body ...) ds)]
    [(_ f:id ds)
     ;; pass to the runtime function
     #'(ds-map-func f ds)]))

;; ds-reduce: AFunc Any Datashell -> Any
;; Reduces the Datashell to a non Datashell type
;; Action: eagerly evaluated, triggers all transformations stored on the datashell and the actor
;; apply the final composed function on the given datashell and return the result
(define-syntax ds-reduce
  (syntax-parser
    #:literals (rsl-lambda)
    ;; Static checking for lambda literals
    [(_ (lambda (args ...) body ...) acc:expr ds)
     #:with l (length (syntax->list #'(args ...)))
     #:fail-unless (= (eval #'l) 2) "lambda must have 2 arguments in ds-reduce"
     ;; reconstruct the lambda, pass to the runtime function
     #'(ds-reduce-func (lambda (args ...) body ...) acc ds)]
    [(_ f acc ds)
     ;; pass to the runtime function
     #'(ds-reduce-func f acc ds)]))

;; define-rsl: String Any -> Any
;; (define-rsl (x x) Expr ... (values Expr ...))
;; Creates a TFunc, given name, one and only one argument, and body of the TFunc
(define-syntax define-rsl
  (syntax-parser
    #:literals (values)
    [(_ (name:id arg1:id) e ... (values r))
     #'(define name (tfunc #'(arg1) #'(e ... (values r))))]))

;; save-ds: Id Datashell -> Void
;; EFFECTS: Always throws an error, see save-ds-top for the valid macro
(define-syntax save-ds
  (syntax-parser
    [(_ e ...)
     #:fail-unless #f "save-ds may only be used at the top level"
     #'(error 'save-ds "save-ds may only be used at the top level")]))

;; compose-rsl: TFunc TFunc -> TFunc
;; Pull apart the tfuncs for the create-rsl macro
(define-syntax compose-rsl
  (syntax-parser
    [(_ f1 f2)
     #'(create-rsl [(tfunc-arg-name f1) (tfunc-arg-name f2)] (tfunc-func-syntax f1) (tfunc-func-syntax f2))]))

;; create-rsl: x x Expr Expr
;; Merge the two exprs, replacing the new-args in body2 with the orig-arg from body1
#;(define-syntax create-rsl
  (syntax-parser
    [(_ [orig-arg new-arg] body1 body2)
    ...]))

;; save-ds-top: String Datashell -> Void
;; EFFECTS: Binds the Datashell to the given identifier in the global scope. Must be used at the top-level.
(define-syntax save-ds-top
  (syntax-parser
    #:literals (mk-datashell ds-map ds-reduce)
    [(_ i:id (mk-datashell e ...))
     #'(begin (define i (mk-datashell e ...)))]
    [(_ i:id (ds-map e ...))
     #'(begin (define i (ds-map e ...)))]
    [(_ i:id e)
     #'(begin (define i e)
              (unless (Datashell? e)
                ;; TODO better error, make it point to this place
                (error 'save-ds "requires an identifier and a Datashell as arguments")))]
    [(_ e ...)
     #'(error 'save-ds "save-ds requires an identifier and a Datashell as arguments")]))

;; -----------------------------------------------------------------------------
;; RUNTIME LIB

;; [Listof Any] -> Datashell
;; Create a Datashell from a given list
(define (mk-datashell data-list)
  (if (list? data-list)
      (Datashell data-list (lambda (any) any))
      (error 'mk-datashell "First arg must be a list")))

;; ds-collect: Datashell -> [Listof Any]
;; Collects the data in a Datashell and returns it.
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
  (cond [(not (procedure-arity-includes? afunc 2))
         (error 'ds-reduce "Invalid reduce function, must take 2 arguments")]
        [(not (Datashell? ds))
         (error 'ds-reduce "Invalid second argument, should be a Datashell")]
        [else (foldl afunc acc (ds))]))

;; apply-tfunc: TFunc -> Error
;; TFuncs should not be applied, but we want them to be funcs so they appear
;; as funcs in the interactions window
(define (apply-tfunc tfunc)
  (error 'RSL "Functions declared with define-rsl must be applied by ds-map"))

;; (Datashell [Listof Any] TFunc)
(struct Datashell (dataset op) #:transparent #:property prop:procedure ds-collect)

;; (tfunc STX)
;; TFunc will be created from define-rsl
(struct tfunc (arg-name func-syntax) #:property prop:procedure apply-tfunc)
