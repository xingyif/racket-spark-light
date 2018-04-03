#lang racket
; Cherry, Alexander
; Xing, Yifan

;; ---------------------------------------------------------------------------------------------------
;; API
(provide
 ;; We want to provide the full racket langauge
 (except-out (all-from-out racket)
             #%module-begin)
 
 (rename-out [rsl-module-begin #%module-begin])
 
 ;; save-ds: Id Datashell -> Void
 ;; EFFECTS: Binds the Datashell to the given identifier in the global scope. Must be used at the top-level.
 save-ds

 ;; ds-map: TFunc Datashell -> Datashell
 ;; Creates a new Datashell with the old Datashell mapped with the given function.
 ;; Doesn't need to actually be provided, is a form inside of save-ds, which is weird
 ds-map

 ;; ds-reduce: AFunc Any Datashell -> Any
 ;; Collects the data in a Datashell, then reduces it with the given function and accumulator.
 ds-reduce

 ;; (define-map-func (Id) Expr ... (values Expr ...))
 define-map-func

 ;; (define-filter-pred (Id) Expr ... (values Expr ...))
 define-filter-pred
 
 ;; ds-collect: Datashell -> [Listof Any]
 ;; Collects the data in a Datashell and returns it.
 ds-collect

 ;; for debugging ONLY to see the internal graph
 get-graph)

;; ---------------------------------------------------------------------------------------------------
;; DEPENDENCY

(require (for-syntax syntax/parse
                     syntax/kerncase
                     racket))

(module+ test (require rackunit))

;; ----------------------------------------------------------------------------------------------
;; IMPLEMENTATION

;; -----------------------------------------------------------------------------

;; SYNTAX (compile time functions)

;; Phase 1
;; -----------------------------------------------------------------------------

(begin-for-syntax

  (require racket
           csv-reading)

  ;; Debugging tool, to view the graph
  (define (get-graph-internal)
    rsl-graph)

  ;; (Datashell [Listof Any] TFunc)
  (struct Datashell (dataset op) #:transparent)

  ;; (rsl-void)
  ;; our unique struct to identify when an item should be filtered during a map
  (struct rsl-void ())

  ;; this graph holds all the data being stored in DS's to avoid multiple DS's needing to allocate
  ;; memory to the same list; values can only be lists
  (define rsl-graph (make-hash))
  
  ;; map-filter-reduce: TFunc (T1 -> T2) T2 [Listof T1] -> T2 
  ;; Combines the given mapping transformation and reducing action into a single reduction in one iteration 
  ;; of the data stored in the rsl-graph with the given key.
  ;; Particularly for composing a map and a filter before calling reduce
  (define (map-filter-reduce tfunc afunc starting-acc data-key)
    (define data (hash-ref rsl-graph data-key))
    ;; create a function that will transform with tfunc then continue the reduction with afunc
    (define (tfunc-filter current-val acc)
      (let ([transformed (tfunc current-val)])
        (cond [(rsl-void? transformed) acc] ; we've filtered it out, skip
              [else (afunc transformed acc)])))
    ;; one iteration
    (foldr tfunc-filter starting-acc data))

  ;; ds-reduce-phase-one: AFunc Any Datashell -> Any
  ;; Execute all queued mapping transformations onto the Datashell's data,
  ;; then reduces the data with the given function.
  (define (ds-reduce-phase-one afunc acc ds)
    ;; unwrap the body of the lambda
    (define tfunc (eval (Datashell-op ds)))
    (define data (syntax->datum (Datashell-dataset ds))) ; Datashells are read in as syntaxes
    (define collected (map-filter-reduce tfunc afunc acc data))
    collected)

  ;; collect-only: Datashell -> [Listof Any]
  ;; Collects the data in a Datashell and returns it.
  (define (collect-only ds)
    (ds-reduce-phase-one cons '() ds)))

;; [Listof Any] -> Datashell
;; Create a Datashell from a given list
(define-for-syntax (mk-datashell stx)
  (syntax-parse stx
    [e
     #:do [(define ds (Datashell #'e #'(lambda (x) x)))]
     ds]))

;; compose-tfunc: TFunc TFunc -> TFunc
;; Pull apart the tfuncs for the create-rsl macro
(define-for-syntax (compose-tfunc stx)
  (syntax-parse stx
    #:literals (lambda)
    ;; f2 in this case is a plain lambda syntax object #'(lambda ...)
    [(f1:id f2)
     #:do [(define error-msg "argument must be an RSL defined transformation or predicate")
           (define f1-data (syntax-local-value #'f1 (thunk (raise-syntax-error 'compose-tfunc error-msg compose-tfunc #'f1))))]
     #:with (lambda (f2-arg) f2-body ...) #'f2
     (syntax-parse f1-data
       #:datum-literals (transformation pred)
       ;; Simple transformation function, compose with existing function
       [(transformation (f1-arg) f1-body ...)
        #'(lambda (f2-arg)
            (let ([f1-arg (let () f2-body ...)])
              (cond [(rsl-void? f1-arg) f1-arg]
                    [else f1-body ...])))]
       ;; Filtering predicate, turn the predicate into filtering func that replaces item with rsl-void
       ;; if false. Compose with existing function.
       [(pred (f1-arg) f1-body ...)
        #'(lambda (f2-arg)
            (let ([f1-arg (let () f2-body ...)])
              (cond [(rsl-void? f1-arg) f1-arg]
                    [((lambda (f1-arg) f1-body ...) f1-arg) f1-arg]
                    [else (rsl-void)])))])]
    [(_ e ...)
     (displayln #'(e ...))
     #'(error "shouldn't have composed this")]))


;; ds-map-phase-one TFunc Datashell -> Datashell
;; Maps the given function on the Datashell and returns a new Datashell
;; Transformation: lazily evaluated. Compose the given function with the previous functions
;; but do not evaluated the given function
;; Example: (ds-map-phase-one add-1 (mk-datashell '(1 2)) -> (Datashell '2 3)
(define-for-syntax (ds-map-phase-one stx)
  (syntax-parse stx
    [(f:id ds)
     #:do [(define f+ (syntax-local-value #'f (thunk (raise-syntax-error 'ds-map "argument must be an RSL defined map or predicate" ds-map-phase-one #'f))))
           (define ds+ (syntax-local-value #'ds))
           (define error-msg-ds "The 2nd argument of ds-map must be an RSL Datashell")
           (unless (Datashell? ds+)
                    (raise-syntax-error 'ds-map error-msg-ds #'ds))
           (define old (Datashell-op ds+))
           (define composed (compose-tfunc #`(f #,old)))
           (define data (Datashell-dataset ds+))
           (define new-ds (Datashell data composed))]
     #`#,new-ds]))

;; Phase 0
;; -----------------------------------------------------------------------------

;; #%module-begin for RSL
(define-syntax rsl-module-begin
  (syntax-parser
    [(_ e ...)
     #'(#%module-begin (validate-top-level e) ...)]))

;; (define-map-func (x x) Expr ... (values Expr ...))
;; Creates a TFunc, given name, one and only one argument, and body of the TFunc
(define-syntax define-map-func
  (syntax-parser 
    [(_ (name:id arg:id) body ...)
     ;; wrap in transformation so compose knows this is a transformation
     #'(define-syntax name #'(transformation (arg) body ...))]))

;; (define-filter-pred (x x) Expr ... (values Expr ...))
;; Creates a TFunc, given name, one and only one argument, and body of the TFunc
;; replaces the boolean returning user function with one that returns either identity
;; or rsl-void
(define-syntax define-filter-pred
  (syntax-parser 
    [(_ (name:id arg:id) body ...)
     ;; wrap in a pred so compose knows this is a predicate
     #'(define-syntax name #'(pred (arg) body ...))]))

;; ds-map: TFunc Datashell -> Datashell
;#:with l (length (syntax->list #'arg))
;#:fail-unless (= (eval #'l) 1) "lambda must have 1 arguments in ds-map"
(define-syntax ds-map
  (syntax-parser
    [(_ f:id ds:id)
      #:do [(define f+ (syntax-local-value #'f (thunk (raise-syntax-error 'ds-map "argument must be an RSL defined map or predicate" ds-map-phase-one #'f))))
           (define ds+ (syntax-local-value #'ds (thunk (raise-syntax-error 'ds-map "argument must be an RSL Datashell" ds-map-phase-one #'ds))))
           (unless (Datashell? ds+)
             (raise-syntax-error 'ds-map "argument must be a Datashell" ds-map-phase-one #'ds))
           ;; call ds-map in phase 1
           (define mapped (ds-map-phase-one #`(f+ ds+)))]
     #`'#,mapped]))

;; ds-reduce: AFunc Any Datashell -> Any
;; Reduces the Datashell to a non Datashell type
;; Action: eagerly evaluated, triggers all transformations stored on the datashell and the actor
;; apply the final composed function on the given datashell and return the result
(define-syntax ds-reduce
  (syntax-parser
    ;; Static checking for lambda literals
    [(_ (lambda (arg1 arg2) body ...) acc:expr ds)
     #:do [(define error-msg-ds "argument must be an RSL Datashell")
           (define reduction (eval #'(lambda (arg1 arg2) body ...)))
           (define datashell (syntax-local-value #'ds (thunk (raise-syntax-error 'ds-collect error-msg-ds #'ds))))
           (unless (Datashell? datashell)
             (raise-syntax-error 'ds-collect error-msg-ds #'ds))
           ;; the transformed data as a list, result from the phase 1 function
           (define reduced (ds-reduce-phase-one reduction (eval #'acc) datashell))]
     ;; reconstruct the lambda, pass to the runtime function
     #`'#,reduced]
    ;; Identifier for function ; TODO, only works with Racket base functions right now
    [(_ f:id acc ds:id)
     #:do [(define error-msg-ds "argument must be an RSL Datashell")
           (define error-msg-f "argument must be a defined reduction function")
           (define reduction (eval #'f))
           (define datashell (syntax-local-value #'ds (thunk (raise-syntax-error 'ds-collect error-msg-ds #'ds))))
           (unless (Datashell? datashell)
             (raise-syntax-error 'ds-collect error-msg-ds #'ds))
           ;; the transformed data as a list, result from the phase 1 function
           (define reduced (ds-reduce-phase-one reduction (eval #'acc) datashell))]
     ;; pass to the runtime function
     #`'#,reduced]))


;; validate-top-level: Syntax -> Syntax
;; Validates top-level forms for our rules and modifies them where needed
(define-syntax validate-top-level
  (syntax-parser
    #:literals (save-ds)
    ;; Allow Datashells to be use at the top level
    [(_ i:id)
     ;; if it's in the our syntax table, use it. Otherwise use the runtime definition
     #:do [(define syntax-val (syntax-local-value #'i (thunk #'i)))]
     #`#,syntax-val]
    ;; Replace all top-level save-ds's with non-error-throwing ones
    [(_ (save-ds e ...))
     #'(save-ds-top e ...)]
    ;; Everything else
    [(_ e)
     (define f (local-expand #'e (syntax-local-context) (kernel-form-identifier-list)))
     (syntax-parse f 
       #:literals (save-ds)
       [(save-ds args ...)
        ;; Replace all top-level save-ds's with non-error-throwing ones
        #'(save-ds-top args ...)]
       [other
        #'e])]))

;; save-ds: Id Datashell -> Void
;; EFFECTS: Always throws an error, see save-ds-top for the valid macro
(define-syntax save-ds
  (syntax-parser
    [(_ e ...)
     #:fail-unless #f "save-ds may only be used as a module-level definition"
     #'(error 'save-ds "save-ds may only be used as a module-level definition")]))

;; save-ds-top: String Datashell -> Void
;; EFFECTS: Binds the Datashell to the given identifier in the global scope. Must be used at the top-level.
(define-syntax save-ds-top
  (syntax-parser
    #:datum-literals (mk-datashell ds-map)
    ;; given a string path
    [(_ i:id (mk-datashell path:string))
     #:do [;; Load csv file into hashmap
           (define error-msg-ds "argument must be a list or a path to a CSV file!")
           (define data (csv->list (open-input-file (eval #'path))))
           (hash-set! rsl-graph (syntax->datum #'i) data)]
     #`(define-syntax i '#,(mk-datashell #'i))]
    ;; given anything not a string path
    [(_ i:id (mk-datashell (e ...)))
     #:do [(define error-msg-ds "argument must be a list or a path to a CSV file!")
           (hash-set! rsl-graph (syntax->datum #'i) (eval #'(e ...)))]
     #`(define-syntax i '#,(mk-datashell #'i))]
    [(_ i:id (ds-map e ...))
     #`(define-syntax i #,(ds-map-phase-one #'(e ...)))]
    [(_ e ...)
     #'(error 'save-ds "save-ds requires an identifier and a Datashell as arguments")]))

;; ds-collect: Datashell -> [List-of Any]
;; Collects the data in a Datashell and returns it.
;; (Pass user input to phase 1)
(define-syntax ds-collect
  (syntax-parser
    [(_ ds:id)
     ;; get the actual datashell
     #:do [(define error-msg "argument must be an RSL Datashell")
           (define datashell (syntax-local-value #'ds (thunk (raise-syntax-error 'ds-collect error-msg #'ds))))
           (unless (Datashell? datashell)
             (raise-syntax-error 'ds-collect error-msg #'ds))
           ;; the transformed data as a list
           (define collected (collect-only datashell))]
     ;; splice the list back into the syntax
     #`'#,collected]))

;; -----------------------------------------------------------------------------
;; RUNTIME LIB

;; Debugging function to display the graph
;; (get-graph)
(define-syntax (get-graph stx)
  (define x (get-graph-internal))
  #`#,x)