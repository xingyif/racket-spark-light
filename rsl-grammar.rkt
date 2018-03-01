#lang racket
#|

-----------RSL Core Grammar-----------:

Program		=	Definition-Expr
 	 	|	...

Definition-Expr =       Definition
                |       Expr

Definition	= 	(define Id Expr)


Expr		=	DataShell
		=	Tranformation
		=	Action
		=	(λ (e) e)
		=	(λ (e) w)
		=	(λ (e loe) w)
		=	All other Racket expressions


DataShell	= 	(DataShell-import Id ref)
		= 	(DataShell Id Tranformation)
		= 	(DataShell Id Action)


Tranformation	= 	(map Expr DataShell)
		= 	(map Expr Tranformation)
		= 	(filter Expr DataShell)
		= 	(filter Expr Tranformation)
		= 	(flatmap Expr DataShell)
		= 	(flatmap Expr Tranformation)


Action		= 	(reduce Expr DataShell)
		= 	(reduce Expr Transformation)
		= 	(collect DataShell)
		= 	(collect Transformation)
		= 	(count DataShell)
		= 	(count Transformation)

Examples:
(DataShell 'result (reduce Expr (DataShell 'b (map Expr (DataShell-import 'a "data.csv")))))
(DataShell 'result (reduce Expr (filter Expr (map Expr (DataShell-import 'a "data.csv")))))


|#