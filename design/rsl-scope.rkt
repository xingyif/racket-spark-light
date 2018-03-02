#lang racket
#|

-----------RSL Core Scope-----------:

(define x e) binds x in the top level scope
(save-ds x datashell) binds x in the top level scope

;; TFunc
(λ (x) e) binds x in e

;; AFunc
(λ (x acc) (+ x acc)) binds x, acc in (+ x acc)

|#
