#lang racket
; Cherry, Alexander
; Xing, Yifan

;; ---------------------------------------------------------------------------------------------------
;; API
(provide
 ;; We want to provide the full racket langauge
 (all-from-out racket)
 
 ;; ds-map: TFunc Datashell -> Datashell
 ;; Creates a new Datashell with the old Datashell mapped with the given function.
 ds-map

 ;; ds-map: TFunc Datashell -> Datashell
 ;; Creates a new Datashell with the old Datashell mapped with the given function.
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

 ;; ds-count: Datashell -> Number
 ;; Counts the number of items in the datashell (after transformations and filters)
 ds-count

 ;; (define-rsl (Id Id) Expr ...)
 ;; EFFECTS: 
 define-rsl-func)

;; ---------------------------------------------------------------------------------------------------
;; DEPENDENCY

(require (for-syntax syntax/parse
                     syntax/kerncase))

(require   
  (prefix-in un: racket)
  csv-reading)

(module+ test (require rackunit))

(define-namespace-anchor rsl)

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION

;; -----------------------------------------------------------------------------
;; SYNTAX (compile time functions)

;; ds-map: TFunc Datashell -> Datashell
;; Maps the given function on the Datashell and returns a new Datashell
;; Transformation: lazily evaluated. Compose the given function with the previous functions
;; but do not evaluated the given function
;; Example: (ds-map (lambda (x) (+ 1 x)) (mk-datashell '(1 2)) -> (Datashell '2 3)
(define-syntax (ds-map stx)
  (syntax-parse stx
    ;; Static checking for lambda literals
    [(_ (lambda (arg1) body ...) ds)
     ;; reconstruct lambda and pass to the runtime function
     #'(ds-map-func '(lambda (arg1) body ...) ds)]
    [(_ f:id ds)
     ;; pass to the runtime function
     #'(ds-map-saved-func f ds)]
    [(this e ...)
     #:do [(raise-syntax-error 'ds-map "wrong" #'this)]
     #'(error 'ds-map "wrong")]))

;; ds-map: TFunc Datashell -> Datashell
;; Maps the given function on the Datashell and returns a new Datashell
;; Transformation: lazily evaluated. Compose the given function with the previous functions
;; but do not evaluated the given function
;; Example: (ds-map (lambda (x) (+ 1 x)) (mk-datashell '(1 2)) -> (Datashell '2 3)
(define-syntax (ds-filter stx)
  (syntax-parse stx
    ;; Static checking for lambda literals
    [(_ (lambda (arg1) body ...) ds)
     ;; reconstruct lambda and pass to the runtime function
     #'(ds-filter-func '(lambda (arg1) body ...) ds)]
    [(_ f:id ds)
     ;; pass to the runtime function
     #'(ds-filter-saved-func f ds)]))

;; ds-reduce: AFunc Any Datashell -> Any
;; Reduces the Datashell to a non Datashell type
;; Action: eagerly evaluated, triggers all transformations stored on the datashell and the actor
;; apply the final composed function on the given datashell and return the result
(define-syntax ds-reduce
  (syntax-parser
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
(define-syntax (define-rsl-func stx)
  (syntax-parse stx
    #:literals (values)
    [(_ (name:id arg1:id) e ...)
     #'(begin
         ;; we have this throwaway lambda to trick DrRacket into showing arrows
         ;; we void it to prevent it from being shown at the top level
         (void (lambda (arg1) e ...))
         ;; the actual saving of the func name for use
         (define name (tfunc '(lambda (arg1) e ...))))]
    [(_ (name:id arg1:id arg-bad:id arg-bad-rest ...) e2 ...)
     #:do [(raise-syntax-error 'define-rsl-func "too many arguments" #'arg-bad)]
     #'(error 'define-rsl-func)]
    [(_ e ...)
     #:do [(raise-syntax-error 'define-rsl-func "incorrect use of define-rsl-func" stx)]
     #'(error 'define-rsl-func)]))

(define-syntax (mk-datashell stx)
  (syntax-parse stx
    [(_ path:string)
     #'(mk-datashell-func (csv->list (open-input-file path)))]
    [(_ data)
     #'(mk-datashell-func data)]
    [(this e ...)
     #:do [(raise-syntax-error 'mk-datashell "incorrect arguments" #'this)]
     #'(error 'mk-datashell "incorrect arguments")]))
;; -----------------------------------------------------------------------------
;; RUNTIME LIB

;; [Listof Any] -> Datashell
;; Create a Datashell from a given list
(define (mk-datashell-func data-list)
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
  (ds-reduce-func cons '() ds))

(define (to-pred-func datum1)
  (match datum1
    [`(lambda (,f1-arg) ,f1-body ...)
     `(lambda (,f1-arg) (if ((lambda (,f1-arg) ,@f1-body) ,f1-arg) ,f1-arg (rsl-void)))]))

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
                    (cond [(rsl-void? ,f1-arg) ,f1-arg]
                          [else ,@f1-body]))))]
       ;; second function is invalid
       [other (throw-error)])]
    ;; first function is invalid
    [other (throw-error)]))

;; ds-map-func: TFunc Datashell -> Datashell
;; Queues up a mapping to later be applied to a Datashell's data.
(define (ds-map-func tfunc-datum ds)
  (Datashell (Datashell-dataset ds) (rsl-compose-to-tfunc tfunc-datum (tfunc-form (Datashell-op ds)))))

;; ds-map-func: TFunc Datashell -> Datashell
;; Queues up a mapping to later be applied to a Datashell's data.
(define (ds-filter-func tfunc-datum ds)
  (Datashell (Datashell-dataset ds) (rsl-compose-to-tfunc (to-pred-func tfunc-datum) (tfunc-form (Datashell-op ds)))))

;; check that the variable stores a tfunc and pass it along
(define (ds-map-saved-func func ds)
  (if (tfunc? func)
      (ds-map-func (tfunc-form func) ds)
      (error 'ds-map "argument is not a valid rsl procedure")))

;; check that the variable stores a tfunc and pass it along
(define (ds-filter-saved-func func ds)
  (if (tfunc? func)
      (ds-filter-func (tfunc-form func) ds)
      (error 'ds-map "argument is not a valid rsl procedure")))

;; ds-reduce-func: AFunc Any Datashell -> Any
;; Execute all queued mapping transformations onto the Datashell's data,
;; then reduces the data with the given function.
(define (ds-reduce-func afunc acc ds)
  (cond [(not (procedure-arity-includes? afunc 2))
         (error 'ds-reduce "Invalid reduce function, must take 2 arguments")]
        [(not (Datashell? ds))
         (error 'ds-reduce "Invalid second argument, should be a Datashell")]
        [else (define composed-datum (tfunc-form (Datashell-op ds)))
              ;; TODO, is there actually a better way to evaluate a symbol?
              ;; This is evaluated at runtime and requires no outside information, so it feels fine...
              (define ns (namespace-anchor->namespace rsl))
              (define composed-tfunc (eval composed-datum ns))
              (define (tfunc-filter current-val acc)
                (let ([transformed (composed-tfunc current-val)])
                  (cond [(rsl-void? transformed) acc] ; we've filtered it out, skip
                        [else (afunc transformed acc)])))
              (foldr tfunc-filter acc (Datashell-dataset ds))]))

(define (ds-count ds)
  (ds-reduce-func (lambda (curr acc) (+ 1 acc)) 0 ds))

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
