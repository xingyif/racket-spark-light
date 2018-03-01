#lang racket
#|

-----------RSL Core Grammar-----------:

Program		=	Definition
 	 	|	...
 	 	|	DataShell
 	 	|	...
 	 	| 	Tranformation
 	 	|	...
 	 	| 	Action


Definition	= 	(define Id Expr)


Expr		=	DataShell
		=	Tranformation
		=	Action
		=	(λ (e) e)
		=	(λ (e) w)
		=	(λ (e loe) w)
		=	All other Racket expressions


DataShell	= 	(DataShell Id ref)


Tranformation	= 	(map Expr DataShell)
		= 	(filter Expr DataShell)
		= 	(flatmap Expr DataShell)


Action		= 	(reduce Expr DataShell)
		= 	(collect DataShell)
		= 	(count DataShell)


|#