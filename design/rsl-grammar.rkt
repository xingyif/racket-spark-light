#lang racket
#|
-----------RSL Core Grammar-----------:


Program		=	Top-level
 	 	|	...

Top-level       =       Definition
                |       Expr

Definition	= 	(define Id RExpr)
                |       (define-map-func (Id Id) RExpr) ;; first Id is the function name, second the arg name
                |       (define-filter-pred (Id Id) RExpr)
                |       (save-ds Id Datashell) ;; Datashell saved as id x

RExpr           =       All expressions from Racket

Expr            = 	RExpr
                |	TExpr

TExpr		=       Datashell
		|	Tranformation
		|	Action

                        ;; TFuncs can only be constructed with define-map-func and define-filter-pred
TFunc		=	(transformation (Id) Expr) 
                =       (pred (Id) Expr) ;; Expr evaluates to a boolean
                        
AFunc		=	(λ (x1 x2) Expr)

FilePath        =       String ;; string describes a system filepath

DataShell	= 	(mk-datashell [Listof Any])
                |       (mk-datashell FilePath) 
                |       Transformation

Tranformation   =       (ds-map TFunc DataShell)
		| 	(ds-flatmap TFunc DataShell) ;; TODO

Action		= 	(ds-reduce  AFunc Expr DataShell) ;; Expr is an accumulator
		| 	(ds-collect DataShell)
		| 	(ds-count   DataShell) ;; TODO

Examples:

1. Import a spreadsheet, transform it, and count the number of items in it
;; Define a datashell as identifier spreadsheet
(save-ds spreadsheet (mk-datashell-csv "data.csv"))
;; Count the number of items in spreadsheet
(ds-count spreadsheet) ;; TODO

2. Transform a list '(1 2 3) by multiplying each number by 10, then divide each number by 5, then add all the items together
(save-ds first-list (mk-datashell (list 1 2 3))
(define-map-func (mul-10 num) (* num 10))
(save-ds mapped-first (ds-map mul-10 first-list))
(define-map-func (div-5 num) (/ num 5))
(save-ds mapped-second (ds-map div-5 mapped-first))
(define my-result (ds-reduce + 0 mapped-second))
;; return 12
|#
