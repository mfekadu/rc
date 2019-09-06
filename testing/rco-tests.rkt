#!/usr/bin/env racket
#lang racket
(require rackunit)
(require "../src/rco.rkt")
(require "utilities.rkt") ; for check-fail and check-fail-with-name

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
;(verify-rco-evals-correctly given5)
#;(check-match (rco given5)
             `(let ([x 20])
                (let ([y x])
                  (+ y 1))))

; same as above
(define given6 '(let ([y (let ([x1 20]) (+ x1 (let ([x2 22]) x2)))]) y))
;(verify-rco-evals-correctly given6)
#;(check-match (rco given6)
             `(let ([x1 20])
                (let ([x2 22])
                  (let ([y (+ x1 x2)])
                    y))))

; r2 tests with new operators 
; this should work on the shrunken r2 language, so we only need to add support for eq?, not, <, and if statements
(define given7 '(eq? (not #t) (< 4 5)))
(verify-rco-evals-correctly given7)
(check-match (rco given7) `(let ([,(? symbol? not-t) (not #t)])
                             (let ([,(? symbol? lt-4-5) (< 4 5)])
                               (eq? ,not-t ,lt-4-5))))

(define given8 '(< (+ 2 8) (+ (- 10) 5)))
(verify-rco-evals-correctly given8)
(check-match (rco given8) `(let ([,(? symbol? plus-2-8) (+ 2 8)])
                             (let ([,(? symbol? neg-10) (- 10)])
                               (let ([,(? symbol? plus-neg-10-5) (+ ,neg-10 5)])
                                 (< ,plus-2-8 ,plus-neg-10-5)))))

; With if statements, it's important that the then case ONLY gets evaluated if the condition is true, and similarly
; the else case ONLY gets evaluated if the condition is false.
; It would be incorrect for this program to look like:
;       (let ([cnd (not #t)])
;         ; pretend these aren't complex
;         (let ([thn (+ 2 (- 1))]))
;           (let ([els (+ 3 (- 2))])
;             (if cnd thn els))))
; because in this case, both the then and else cases get evaluated even if the condition says otherwise
(define given9 '(if (not #t) (+ 2 (- 1)) (+ 3 (- 2))))
(verify-rco-evals-correctly given9)
(check-match (rco given9) `(let ([,(? symbol? cnd) (not #t)])
                             (if ,cnd
                               (let ([,(? symbol? neg-1) (- 1)]) (+ 2 ,neg-1))
                               (let ([,(? symbol? neg-2) (- 2)]) (+ 3 ,neg-2)))))

; Trying to handle nested if statements
(define given10 '(< (+ 4 5) (if (not #f) (+ 2 3) (- 4))))
(verify-rco-evals-correctly given10)
(check-match (rco given10) `(let ([,(? symbol? plus-4-5) (+ 4 5)])
                              (let ([,(? symbol? if-stmt)
                                      (let ([,(? symbol? cnd) (not #f)])
                                        (if ,cnd
                                          (+ 2 3)
                                          (- 4)))])
                                (< ,plus-4-5 ,if-stmt))))

; if nested within if
(define given11 '(if (eq? 1 (- 1))
                   (if (not #f) (+ 2 1) (+ 3 (- 2)))
                   (if (not #t) (+ 4 (- 5)) (+ 2 (- 3)))))
(verify-rco-evals-correctly given11)
(check-match (rco given11) `(let ([,(? symbol? neg1) (- 1)])
                              (let ([,(? symbol? cnd1) (eq? 1 ,neg1)])
                                (if ,cnd1
                                  ; then case
                                  (let ([,(? symbol? cnd2) (not #f)])
                                    (if ,cnd2
                                      ;then case
                                      (+ 2 1)
                                      ; else case
                                      (let ([,(? symbol? neg2) (- 2)])
                                        (+ 3 ,neg2))))
                                  ; else case
                                  (let ([,(? symbol? cnd3) (not #t)])
                                    (if ,cnd3
                                      ; then case
                                      (let ([,(? symbol? neg5) (- 5)]) 
                                        (+ 4 ,neg5))
                                      ; else case
                                      (let ([,(? symbol? neg3) (- 3)])
                                        (+ 2 ,neg3))))))))

(define given12 '(if (if (< 1 2) #t #f) (+ 1 2) (+ 3 4)))
(verify-rco-evals-correctly given12)
(check-match (rco given12) `(let ([,(? symbol? nested-if-cnd) 
                                     (let ([,(? symbol? inner-cnd) (< 1 2)]) (if ,inner-cnd #t #f))])
                               (if ,nested-if-cnd (+ 1 2) (+ 3 4))))

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
