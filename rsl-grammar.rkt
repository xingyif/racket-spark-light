#lang racket
#|
-----------RSL Core Grammar-----------:


Program		=	Top-level
 	 	|	...

Top-level       =       Definition
                |       Expr

Definition	= 	(define x RExpr)
                |       (save-ds x Datashell) ;; Datashell saved as id x

RExpr           =       All expressions from Racket

Expr            = 	RExpr
                |	TExpr
		|	TFunc

TExpr		=       DataShell
		|	Tranformation
		|	Action

TFunc		=	(λ (x) Expr)
		|	(λ (x) Expr)

AFunc		=	(λ (x1 x2) Expr)

FilePath        =       String ;; string describes a system filepath

DataShell	= 	(mk-datashell [Listof Any]) ;; x is a new name for this datashell
                |       (mk-datashell-csv FilePath) 
                |       Transformation

Tranformation   =       (ds-map TFunc DataShell)
		| 	(ds-filter TFunc DataShell)
		| 	(ds-flatmap TFunc DataShell)

Action		= 	(ds-reduce  AFunc DataShell)
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
