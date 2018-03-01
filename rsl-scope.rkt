#lang racket
#|

-----------RSL Core Scope-----------:

(define x e) binds x in e

(λ (x) e) binds x in e

(λ (x acc) (+ x acc)) binds x, acc in (+ x acc)

|#
