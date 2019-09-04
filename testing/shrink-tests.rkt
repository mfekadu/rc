#!/usr/bin/env racket
#lang racket
(require rackunit)
(require racket/contract)
(require "utilities.rkt") ; for check-fail and check-fail-with-name
(require "../src/shrink.rkt")

(check-equal? (shrink-exp '(- x y)) '(+ x (- y)))
(check-equal? (shrink-exp '(<= x y)) '(if (< x y) #t (eq? x y)))
(check-equal? (shrink-exp '(> x y)) '(if (< x y) #f (not (eq? x y))))
(check-equal? (shrink-exp '(>= x y)) '(not (< x y)))

; and, or
(check-equal? (shrink-exp '(and x y)) '(if (eq? x #t) (eq? y #t) #f))
(check-equal? (shrink-exp '(or x y)) '(if (eq? x #f) (eq? y #t) #t))

(check-equal? (shrink-exp '(if (and x y) (- 3 2) (+ 1 1))) 
                          '(if (if (eq? x #t) (eq? y #t) #f)
                             (+ 3 (- 2))
                             (+ 1 1)))

(check-equal? (shrink-exp '(>= (- x y) (+ x y)))
              '(not (< (+ x (- y)) (+ x y))))


(check-equal? (shrink-exp '(read)) '(read))

(check-equal? (shrink-exp '(let [[x 3]] 42)) '(let [[x 3]] 42))

(check-equal? (shrink-exp '(let [[x (- 10 4)]] (>= 42 42))) '(let [[x (+ 10 (- 4))]] (not (< 42 42))))

(check-equal? (shrink-exp #f) #f)
(check-equal? (shrink-exp #t) #t)
(check-equal? (shrink-exp 42) 42)
(check-equal? (shrink-exp 'x) 'x)


(check-equal? (shrink-exp '(- (read))) '(- (read)))
(check-equal? (shrink-exp '(- 42)) '(- 42))


(check-equal? (shrink `(program () (read))) `(program () (read)))
(check-fail (λ () (shrink `(program () ()))))
(check-fail (λ () (shrink `(malformed () ()))))

(displayln "shrink tests finished")