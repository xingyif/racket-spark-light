#lang racket
#|
-----------RSL Core Grammar-----------:


Program		=	Top-level
 	 	|	...

Top-level       =       Definition
                |       Expr

Definition	= 	(define x RExpr)
                |       (define-map-func x RExpr)
                |       (define-filter-pred x RExpr)
                |       (save-ds x Datashell) ;; Datashell saved as id x

RExpr           =       All expressions from Racket
                |       DSFunc   ;; DSFunc's are just signature restricted Racket lambdas

Expr            = 	RExpr
                |	TExpr

TExpr		=       Datashell
		|	Tranformation
		|	Action

DSFunc          =       TFunc
                =       AFunc

TFunc		=	(λ (x) Expr)
                        

AFunc		=	(λ (x1 x2) Expr)

FilePath        =       String ;; string describes a system filepath

DataShell	= 	(mk-datashell [Listof Any])
                |       (mk-datashell FilePath) 
                |       Transformation

Tranformation   =       (ds-map TFunc DataShell)
		| 	(ds-filter TFunc DataShell)
		| 	(ds-flatmap TFunc DataShell)

Action		= 	(ds-reduce  AFunc Expr DataShell) ;; Expr is an accumulator
		| 	(ds-collect DataShell)
		| 	(ds-count   DataShell)

Examples:

1. Import a spreadsheet, transform it, and count the number of items in it
;; Define a datashell as identifier spreadsheet
(save-ds spreadsheet (mk-datashell-csv "data.csv"))
;; Count the number of items in spreadsheet
(ds-count spreadsheet)

2. Transform a list '(1 2 3) with various mapping and filters, then add all the items together
(save-ds first-list (mk-datashell (list 1 2 3))
;; ds-filters can be shifted
(save-ds even-shifted (ds-map add1 (ds-filter even? (ds-map add1 first-list))))
(define my-result (ds-reduce add-acc even-shifted))

|#
