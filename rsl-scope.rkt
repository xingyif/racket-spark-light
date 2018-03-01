#lang racket
#|

-----------RSL Core Scope-----------:

(define original-ds (DataShell-import 'a "data.csv")) binds original-ds in (DataShell-import 'a "data.csv")

(define map-ds (map expr original-ds)) binds map-ds in (map expr original-ds)
(define reduce-map-ds (reduce expr map-ds)) binds reduce-map-ds in (reduce expr map-ds)

(define filter-map-ds (filter expr map-ds)) binds filter-map-ds in (filter expr map-ds)
(define collect-filter-map-ds (collect expr filter-map-ds))
binds collect-filter-map-ds in (collect expr filter-map-ds)

(λ (x) e) binds x in e

(λ (x acc) (+ x acc)) binds x, acc in (+ x acc)

|#
