#!/usr/local/bin/racket
#lang racket
(require rackunit)
(require "../src/rco.rkt")
(require "test-helpers.rkt") ; for check-fail and check-fail-with-name

; TEST HELPERS
(define make-list-from-vals (λ (a b) (list a b)))

(define (verify-rco-arg-output-is-empty given)
  (check-match
    (call-with-values (λ () (rco-arg given)) make-list-from-vals)
    (list given (? empty?))))

(define (verify-rco-arg-output given expect)
  (check-match
    (call-with-values (λ () (rco-arg given)) make-list-from-vals)
    (list (? symbol? s) (list (? symbol? s) expect))))

; TEST CASES
(check-equal? (rco '(+ 2 2)) '(+ 2 2))

(check-match (rco '(+ 2 (- 3)))
             `(let ([,(? symbol? s) (- 3)]) (+ 2 ,(? symbol? s))))

(check-match  (rco '(+ (- 2) 3))
             `(let ([,(? symbol? s) (- 2)]) (+ ,(? symbol? s) 3)))

(check-match (rco '(+ (- 2) (- 3)))
             `(let ([,(? symbol? s1) (- 2)]) (let ([,(? symbol? s2) (- 3)]) (+ ,(? symbol? s1) ,(? symbol? s2)))))

(check-match (rco '(+ (- (- 2)) 3))
             `(let ([,(? symbol? s1) (- 2)]) (let ([,(? symbol? s2) (- ,s1)]) (+ ,s2 3))))

(check-match (rco '(+ (- (- 2)) (+ 3 (- 4))))
             `(let ([,(? symbol? neg2) (- 2)])
                (let ([,(? symbol? negneg2) (- ,neg2)])
                  (let ([,(? symbol? neg4) (- 4)])
                    (let ([,(? symbol? plus3neg4) (+ 3 ,neg4)])
                      (+ ,negneg2 ,plus3neg4))))))

; setup namespace for eval
; https://stackoverflow.com/a/37246112
(current-namespace (make-base-namespace))

; test does exactly what you think it does
(define (verify-rco-evals-correctly given)
  (check-equal? (eval (rco given)) (eval given)))

;
(define given '(let ([x 1]) x))
(verify-rco-evals-correctly given)
(check-equal? (rco given) given)

;
(define given2 '(+ 4 (let ([x 1]) x)))
(verify-rco-evals-correctly given2)
(check-equal? (rco given2)
              '(let ([x 1]) (+ 4 x)))

;
(define given3 '(+ (let ([x (- (- 1))]) (+ x (- 2))) 40))
(verify-rco-evals-correctly given3)
(check-match (rco given3)
             `(let ([,(? symbol? neg1) (- 1)])
                (let ([x (- ,neg1)])
                  (let [[,(? symbol? neg2) (- 2)]]
                    (let [[,(? symbol? plusxneg2) (+ x ,neg2)]] (+ ,plusxneg2 40))))))


(define given4 '(let ([a 42]) (let ([b a]) b)))
(verify-rco-evals-correctly given4)
(check-equal? (rco given4) given4)

; rco output is correct but not optimal - extra temp is introduced
; this test is meant to fail
(define given5 '(let ([y (let ([x 20]) x)]) (+ y 1)))
(verify-rco-evals-correctly given5)
(check-match (rco given5)
             `(let ([x 20])
                (let ([y x])
                  (+ y 1))))

; same as above
(define given6 '(let ([y (let ([x1 20]) (+ x1 (let ([x2 22]) x2)))]) y))
(verify-rco-evals-correctly given6)
(check-match (rco given6)
             `(let ([x1 20])
                (let ([x2 22])
                  (let ([y (+ x1 x2)])
                    y))))


; testing rco-prog
(define given3-prog `(program () ,given3))
(check-match (rco-prog given3-prog)
             `(program () (let ([,(? symbol? neg1) (- 1)])
                                    (let ([x (- ,neg1)])
                                      (let [[,(? symbol? neg2) (- 2)]]
                                        (let [[,(? symbol? plusxneg2) (+ x ,neg2)]] (+ ,plusxneg2 40)))))))

; test bad rco-prog inputs
(check-fail (λ () (rco-prog #t)))
(check-fail (λ () (rco-prog rco-prog)))
; R1 does not have labels
(check-fail (λ () (rco-prog '(program () (start (+ 2 2))))))


; TEST rco-prog simple
(check-equal? (rco-prog '(program () (+ 2 2)))
                '(program () (+ 2 2)))

; TEST rco-prog complex
(check-match (rco-prog '(program () (+ (- 1) 2)))
                `(program () (let [[,(? symbol? tmp) (- 1)]] (+ ,tmp 2))))

(displayln "rco tests finished")
