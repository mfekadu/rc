#!/usr/local/bin/racket
#lang racket
(require rackunit)
(require racket/contract)
(require "test-helpers.rkt") ; for check-fail and check-fail-with-name
(require "../src/uniquify.rkt")

; tests for uniquify-exp
(define uniquify-exp-func (uniquify-exp '()))

(define given1 '(+ 2 2))
(define expect1 '(+ 2 2))
(check-equal? (uniquify-exp-func given1) expect1)

; renaming should work
(define given2 '(let ([x 2]) (+ 2 x)))
(check-match (uniquify-exp-func given2)
              `(let ([,(? symbol? s) 2]) (+ 2 ,s)))

(define given3 '(let ([x 1]) (let ([x x]) (+ x x))))
(check-match (uniquify-exp-func given3)
              `(let ([,(? symbol? s1) 1]) 
                 (let ([,(? symbol? s2) ,s1])
                   (+ ,s2 ,s2))))

; should fail since x is not defined
(define given4 '(+ x 2))
(check-fail (lambda () (uniquify-exp-func given4)))

(define given5 '(let ([x 5]) (+ y 3)))
(check-fail (lambda () (uniquify-exp-func given5)))

(displayln "uniquify tests finished")
