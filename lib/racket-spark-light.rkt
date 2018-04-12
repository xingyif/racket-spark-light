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

 ds-filter
 
 ;; ds-reduce: AFunc Any Datashell -> Any
 ;; Collects the data in a Datashell, then reduces it with the given function and accumulator.
 ds-reduce
 
 ;; mk-datashell: [Listof Any] -> Datashell
 ;; Creates a Datashell data structure from a list of items.
 mk-datashell
 
 ;; ds-collect: Datashell -> [Listof Any]
 ;; Collects the data in a Datashell and returns it.
 ds-collect
 
 ;; define-datashell: Id Datashell -> Void
 ;; EFFECTS: Binds the Datashell to the given identifier in the global scope. Must be used at the top-level.
 define-datashell

 ;; (define-rsl (x x) Expr ... (values Expr ...))
 define-rsl-func)

;; ---------------------------------------------------------------------------------------------------
;; DEPENDENCY

(require (for-syntax syntax/parse
                     syntax/kerncase))

(require   
  (prefix-in un: racket)
  csv-reading)

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
;; Validates top-level define-datashell's
(define-syntax validate-top-level
  (syntax-parser
    #:literals (define-datashell)
    [(_ (define-datashell e ...))
     #'(define-datashell-valid e ...)]
    [(_ e)
     (define f (local-expand #'e (syntax-local-context) #f))
     (syntax-parse f 
       #:literals (define-datashell)
       [(define-datashell args ...)
        #'(define-datashell-valid args ...)]
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
    [(_ (lambda (arg1) body ...) ds)
     ;; reconstruct lambda and pass to the runtime function
     #'(ds-map-func '(lambda (arg1) body ...) ds)]
    [(_ f:id ds)
     ;; pass to the runtime function
     #'(ds-map-saved-func f ds)]))

;; ds-map: TFunc Datashell -> Datashell
;; Maps the given function on the Datashell and returns a new Datashell
;; Transformation: lazily evaluated. Compose the given function with the previous functions
;; but do not evaluated the given function
;; Example: (ds-map (lambda (x) (+ 1 x)) (mk-datashell '(1 2)) -> (Datashell '2 3)
(define-syntax (ds-filter stx)
  (raise-syntax-error 'ds-filter "not implemented"))

;; ds-reduce: AFunc Any Datashell -> Any
;; Reduces the Datashell to a non Datashell type
;; Action: eagerly evaluated, triggers all transformations stored on the datashell and the actor
;; apply the final composed function on the given datashell and return the result
(define-syntax ds-reduce
  (syntax-parser
    #:literals (rsl-lambda)
    ;; Static checking for lambda literals
    [(_ (lambda (arg1 arg2) body ...) acc:expr ds)
     ;; reconstruct the lambda, pass to the runtime function
     #'(ds-reduce-func (lambda (arg1 arg2) body ...) acc ds)]
    [(_ f:id acc ds)
     ;; pass to the runtime function
     #'(ds-reduce-func f acc ds)]))

;; define-rsl: String Any -> Any
;; (define-rsl (x x) Expr ... (values Expr ...))
;; Creates a TFunc, given name, one and only one argument, and body of the TFunc
(define-syntax define-rsl-func
  (syntax-parser
    #:literals (values)
    [(_ (name:id arg1:id) e ...)
     #'(define name (tfunc '(lambda (arg1) e ...)))]))

;; define-datashell: Id Datashell -> Void
;; EFFECTS: Always throws an error, see define-datashell-valid for the valid macro
(define-syntax define-datashell
  (syntax-parser
    [(_ e ...)
     #:fail-unless #f "define-datashell may only be used at the top level"
     #'(error 'define-datashell "define-datashell may only be used at the top level")]))

;; define-datashell-valid: String Datashell -> Void
;; EFFECTS: Binds the Datashell to the given identifier in the global scope. Must be used at the top-level.
(define-syntax define-datashell-valid
  (syntax-parser
    #:literals (mk-datashell ds-map ds-reduce)
    [(_ i:id (mk-datashell path:string))
     #'(define i (mk-datashell (csv->list (open-input-file path))))]
    [(_ i:id (mk-datashell e))
     #'(define i (mk-datashell e))]
    [(_ i:id (ds-map e ...))
     #'(define i (ds-map e ...))]
    [(_ i:id e)
     #'(begin (define i e)
              (unless (Datashell? e)
                ;; TODO better error, make it point to this place
                (error 'define-datashell-valid "requires an identifier and a Datashell as arguments")))]
    [(_ e ...)
     #'(error 'define-datashell "define-datashell requires an identifier and a Datashell as arguments")]))

;; -----------------------------------------------------------------------------
;; RUNTIME LIB

;; [Listof Any] -> Datashell
;; Create a Datashell from a given list
(define (mk-datashell data-list)
  (if (list? data-list)
      (Datashell data-list (tfunc '(lambda (any) any)))
      (error 'mk-datashell "First arg must be a list")))

(define-syntax unwrap
  (syntax-parser
    #:datum-literals (lambda)
    ['(lambda (x) t) #'(lambda (x) t)]))

;; ds-collect: Datashell -> [Listof Any]
;; Collects the data in a Datashell and returns it.
(define (ds-collect ds)
  ;; apply composed function to the stored list, then return the list
  (define composed-datum (tfunc-form (Datashell-op ds)))
  ;; TODO, is there actually a better way to evaluate a symbol?
  ;; This is evaluated at runtime and requires no outside information, so it feels fine...
  (define composed-function (eval composed-datum (module->namespace 'racket)))
  (define mapped (map composed-function (Datashell-dataset ds)))
  mapped)

;; rsl-compose Symbol Symbol -> Symbol
;; compose two functions represented as symbols
(define (rsl-compose-to-tfunc datum1 datum2)
  ;; defined error to use in multiple places
  (define throw-error (thunk (error 'ds-map "argument is not in correct form")))
  ;; match to make sure the first arg is the right shape
  (match datum1
    [`(lambda (,f1-arg) ,f1-body ...)
     ;; match to make sure the second arg is the right shape
     (match datum2
       [`(lambda (,f2-arg) ,f2-body ...)
        (tfunc `(lambda (,f2-arg)
           (let ([,f1-arg (let () ,@f2-body)])
             ,@f1-body)))]
       ;; second function is invalid
       [other (throw-error)])]
    ;; first function is invalid
    [other (throw-error)]))

;; ds-map-func: TFunc Datashell -> Datashell
;; Queues up a mapping to later be applied to a Datashell's data.
(define (ds-map-func tfunc-datum ds)
  (Datashell (Datashell-dataset ds) (rsl-compose-to-tfunc tfunc-datum (tfunc-form (Datashell-op ds)))))

;; check that the variable stores a tfunc and pass it along
(define (ds-map-saved-func func ds)
  (if (tfunc? func)
      (ds-map-func (tfunc-form func) ds)
      (error 'ds-map "argument is not a valid rsl procedure")))

;; ds-reduce-func: AFunc Any Datashell -> Any
;; Execute all queued mapping transformations onto the Datashell's data,
;; then reduces the data with the given function.
(define (ds-reduce-func afunc acc ds)
  (cond [(not (procedure-arity-includes? afunc 2))
         (error 'ds-reduce "Invalid reduce function, must take 2 arguments")]
        [(not (Datashell? ds))
         (error 'ds-reduce "Invalid second argument, should be a Datashell")]
        [else (foldl afunc acc (ds-collect ds))]))

;; ignore-rsl-void
;; Takes an item and the function to apply to it, calls the function
;; if the item isn't an rsl-void
(define (ignore-rsl-void item callback)
  (if (rsl-void? item)
      item
      (callback item)))

;; filter-out
;; Checks an item against a predicate and returns it if true or the rsl-void if false
(define (filter-out item pred)
  (if (pred item)
      item
      (rsl-void)))

;; apply-tfunc: TFunc -> Error
;; TFuncs should not be applied, but we want them to be funcs so they appear
;; as funcs in the interactions window
(define (apply-tfunc tfunc)
  (error 'RSL "Functions declared with define-rsl must be applied by ds-map"))

;; (tfunc STX)
;; TFunc will be created from define-rsl
(struct tfunc (form) #:property prop:procedure apply-tfunc)

;; (Datashell [Listof Any] TFunc)
(struct Datashell (dataset op) #:transparent)

;; for use in filtering
(struct rsl-void ())
