#lang racket
#|

-----------RSL Core Grammar-----------:

Program		=	top-level
 	 	|	...

Top-level       =       Definition
                |       TExpr
                |       Expr (from Racket)

Definition	= 	(define Id TExpr)

TExpr		=       DataShell
		=	Tranformation
		=	Action

TFunc		=	(λ (x) Expr)
		=	(λ (x) Expr)

AFunc		=	(λ (x1 x2) Expr)

DataShell	= 	(DataShell-import Id ref)
		= 	(DataShell x)
		= 	(DataShell x Action)

Tranformation   =       (ds-map TExpr DataShell)
		= 	(ds-map TFunc Tranformation) 
		= 	(ds-filter TFunc DataShell)
		= 	(ds-filter TFunc Tranformation)
		= 	(ds-flatmap TFunc DataShell)
		= 	(ds-flatmap TFunc Tranformation)

Action		= 	(ds-reduce AFunc DataShell)
		= 	(ds-reduce AFunc Transformation)
		= 	(ds-collect DataShell)
		= 	(ds-collect Transformation)
		= 	(ds-count DataShell)
		= 	(ds-count Transformation)

Examples:
(DataShell 'result (reduce Expr (DataShell 'b (map Expr (DataShell-import 'a "data.csv")))))
(DataShell 'result (reduce Expr (filter Expr (map Expr (DataShell-import 'a "data.csv")))))


|#