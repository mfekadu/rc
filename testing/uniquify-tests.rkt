#!/usr/bin/env racket
#lang racket
(require rackunit)
(require racket/contract)
(require "utilities.rkt") ; for check-fail and check-fail-with-name
(require "../src/uniquify.rkt")

(define given1 '(+ 2 2))
(define expect1 '(+ 2 2))
(check-equal? (uniquify-exp given1 '()) expect1)

; renaming should work
(define given2 '(let ([x 2]) (+ 2 x)))
(check-match (uniquify-exp given2 '())
              `(let ([,(? symbol? s) 2]) (+ 2 ,s)))

(define given3 '(let ([x 1]) (let ([x x]) (+ x x))))
(check-match (uniquify-exp given3 '())
              `(let ([,(? symbol? s1) 1])
                 (let ([,(? symbol? s2) ,s1])
                   (+ ,s2 ,s2))))

; should fail since x is not defined
(define given4 '(+ x 2))
(check-fail (lambda () (uniquify-exp given4 '())))

(define given5 '(let ([x 5]) (+ y 3)))
(check-fail (lambda () (uniquify-exp given5 '())))

; test bad uniquify-exp inputs
(check-fail (λ () (uniquify-exp #t '())))
(check-fail (λ () (uniquify-exp uniquify-exp '())))

; test bad uniquify inputs
(check-fail (λ () (uniquify #t)))
(check-fail (λ () (uniquify uniquify-exp)))
; R1 does not have labels
(check-fail (λ () (uniquify '(program () (start (+ 2 2))))))


; TEST uniquify
(check-equal? (uniquify '(program () (+ 2 2)))
                '(program () (+ 2 2)))

(define given6expr '(let ([x 32]) (+ (let ([x 10]) x) x)))
(define given6 `(program () ,given6expr))
(check-match (uniquify given6)
             `(program () (let ([,x.1 32]) (+ (let ([,x.2 10]) ,x.2) ,x.1))))

(displayln "uniquify tests finished")