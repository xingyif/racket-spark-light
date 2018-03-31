#lang s-exp "../lib/racket-spark-light.rkt"

(require (for-syntax syntax/parse))
(require rackunit)

;; Test 1: Mapping small quantities of numbers w/ internal mutation
;; TFuncs
(define-map-func (add-5 y)
  (define x 100)
  (set! x 5)
  (+ y x))

(define-map-func (add-2 y)
  (define z 2)
  (+ y z))

(define-map-func (sub-8 z)
  (- z 8))

#;(ds-map add-5 a)

;; Transformation Applications
(save-ds a (mk-datashell '(5 2)))
(save-ds ab (ds-map add-5 a)) ; Add 5: (10 7)
(save-ds abc (ds-map add-2 ab)) ; Add 2: (12 9)
(save-ds abcd (ds-map sub-8 abc)) ; Subtract 8: (4 1)

;; Failure Example
;; won't work because a mk-datashell only takes a list or a path to a csv file
;(save-ds a (mk-datashell 5))

;; Collect the data in the Datashell
#;(ds-collect abcd)

(check-equal? (ds-collect a) '(5 2))
(check-equal? (ds-collect ab) '(10 7))
(check-equal? (ds-collect abc) '(12 9))
(check-equal? (ds-collect abcd) '(4 1))

;; Test 2: Mapping small quantities of numbers w/ printing to prove single iteration
;; TFuncs
(define-map-func (sub-3-print num)
  (display "t1 ")
  (- num 3))

(define-map-func (mult-2-print num)
  (display "t0 ")
  (* num 2))

;; Transformation Applications
(save-ds 15-range (mk-datashell '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14)))
(save-ds 15-range-middle (ds-map mult-2-print 15-range)) ;; 
(save-ds 15-range-final (ds-map sub-3-print 15-range-middle))

;; Racket's mapping will print "t0" 15 times, then print "t1" 15 times since it iterates twice.
(define 15-range-final-racket  (map (lambda (x) (display "t1,") (- x 3)) (map (lambda (y) (display "t0,") (* 2 y)) (range 15))))

;; RSL's mapping will print alternating "first func" and "second func" as the functions are merged, and iteration only occurs once
#;(ds-collect 15-range-final)

;(check-equal? (ds-collect 15-range-final) '(-1 1 3 5 7 9 11 13 15 17 19 21 23 25 27))
(check-equal? (ds-collect 15-range-final) 15-range-final-racket)

;; Test 3: Mapping small quantities of numbers w/ global mutation (DOES NOT WORK, Should it?)

;; Global variable to mutate
(define global-1 0)

;; TFuncs
(define-map-func (global-1++ num)
  (set! global-1 (+ global-1 1))
  num)

(define-map-func (add-global-1-- num)
  (let ([global-val global-1])
    (set! global-1 (- global-1 1))
    (+ num global-val)))

;; Transformation Applications
(save-ds 10-range (mk-datashell '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))
(save-ds 10-range-middle (ds-map global-1++ 15-range)) ;; 
(save-ds 10-range-final (ds-map add-global-1-- 15-range-middle))

;; BROKEN
;; global-1 is not accessible inside the rsl-func's, bad static error when this is uncommented
;; each rsl-func name and datashell name is stored as a syntax variable, so it is a phase error
#;(ds-collect 10-range-final)

;; Test 4: Mapping small quantities of numbers w/ internal mutation and FILTER

;; tfuncs
(define-filter-pred (less-than-5? num)
  (if #t (< num 5) #f))

(define-map-func (mult-10 num)
  (* 10 num))

;; afuncs

(define-filter-pred (multiple-of-20? num)
  (= (modulo num 20) 0))

;; Transformation Applications
(save-ds a2 (mk-datashell '(1 2 3 4 5 6 7 8 9 10)))
(save-ds ab2 (ds-map less-than-5? a2)) 
(save-ds abc2 (ds-map mult-10 ab2))
(save-ds abcd2 (ds-map multiple-of-20? abc2))

#;(eval cons)
#;(eval sub-3-print)

#;(ds-map sub-3-print a2)

(ds-collect abcd2)
(ds-reduce cons '() abcd2)
#;(ds-map (lambda (x) (+ x 1)) a2)

(check-equal? (ds-collect abcd2) '(20 40))
(check-equal? (append (ds-collect abcd2) '(5)) (ds-reduce cons (cons 5 '()) abcd2))
(check-equal? (ds-reduce + 0 abcd2) 60)
(check-equal? (ds-reduce + (+ 2 3) abcd2) 65)
(check-equal? (ds-reduce (lambda (x y) (+ x y)) (+ 2 3) abcd2) 65)

;; Nice error! It even points to the issue in this file.
#;(save-ds l (ds-map less-than-5? less-than-5?))

