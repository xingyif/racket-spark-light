#lang racket

;; Transformation Type (T-Type)
;; A T-Type is one:
;;   "Map"
;;   "Filter"

;; A T-Func is (-> Any Any)
;; A Transformation is: (Transformation T-Type T-Func)

;; This is it
;; A datashell is: (Datashell [Listof Transformation] [Listof Any] Datashell)
