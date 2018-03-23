#lang racket

(begin-for-syntax
  (struct rsl-func (agrs body)
          )
(define-syntax rs1 (rsl-func #'(x) #'(+ x 1)))



