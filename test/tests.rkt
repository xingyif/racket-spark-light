#lang s-exp "../lib/racket-spark-light.rkt"

;; to make test macros
(require (for-syntax syntax/parse))
(require rackunit)

(define-rsl-func (add-3 num)
  (+ num 3))

(define-rsl-func (mult-2 num)
  (* num 2))

(define-datashell x (mk-datashell '(12 2)))
(define-datashell xy (ds-map add-3 x))
(define-datashell xyz (ds-map mult-2 xy))

(define-datashell xz (ds-map (lambda (i) (+ 3 i)) x))

(check-equal? (ds-collect x) '(12 2))
(check-equal? (ds-collect xy) '(15 5))
(check-equal? (ds-collect xy) (ds-collect xz))
(check-equal? (ds-collect xyz) '(30 10))

(define-rsl-func (print-1 num)
  (display 1)
  (display " ")
  num)

(define-rsl-func (print-2 num)
  (display 2)
  (display " ")
  num)

(define-datashell a (mk-datashell '(a b c d)))
(define-datashell ab (ds-map print-1 a))
(define-datashell abc (ds-map print-2 ab))

;; this will print 
(check-equal? (ds-collect abc) '(a b c d))

;; check that a basic reduce works
(define reversed (ds-reduce (lambda (curr acc) (append acc (list curr))) '() abc))
(check-equal? reversed (reverse (ds-collect abc)))

;; we need to stop globally accessible variables maybe
(define shouldnt-work 5)
(define reversed-bad (ds-reduce (lambda (curr acc) (cons shouldnt-work acc)) '() abc))
reversed-bad

(define-rsl-func (filter-b to-check)
  (not (symbol=? to-check 'b)))

(define-datashell a-filtered (ds-filter filter-b a))
(ds-collect a-filtered)

(define-rsl-func (only-evens checking)
  (= (modulo checking 2) 0))

(define-datashell evens-plus-3 (ds-map add-3 (ds-filter only-evens (mk-datashell '(1 2 3 4 5)))))

(check-equal? (ds-collect evens-plus-3) '(5 7))

(define-datashell csv (mk-datashell "nhs.csv"))
(check-equal? (ds-count csv) 15029)
