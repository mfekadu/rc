#!/usr/bin/racket
#lang racket
(require rackunit)

; Given an expr in R1, return an expr in R1 without any complex subexpressions
(define (rco-exp e) ; returns expr
  (match e
    ; handle base cases
    [(or (? symbol?) (? integer?) '(read)) e]))

 ; Given an expr in R1, return a temp symbol name and an alist mapping from the symbol name to an expr in R1
(define (rco-arg e) ; returns expr, alist
  (match e
    ; handle simple base cases
    [(or (? symbol?) (? integer?) '(read)) e]
    [(list op args ...) (let ([tmp-name (gensym 'tmp)])
                          (values tmp-name
                                  ((list tmp-name e))))]))

; TEST CASES
; ATOMS should stay simple rco-exp
(check-equal? (rco-exp 2) 2)
(check-equal? (rco-exp '+) '+)
; ATOMS should stay simple rco-arg
(check-equal? (rco-arg 2) 2)
(check-equal? (rco-arg '+) '+)
;
;; BAD exprs rco-exp
;(check-equal? (rco-exp (list 2)) "panic!")
;(check-equal? (rco-exp '(x)) "panic!")
;(check-equal? (rco-exp (list '+)) "panic!")
;(check-equal? (rco-exp #t) "panic!")
;; BAD exprs rco-arg
;(check-equal? (rco-arg #t) "panic!")
;
;; SIMPLE exprs SHOULD STAY SIMPLE
;(check-equal? (rco-exp (list '+ 2 2)) '(+ 2 2))
;(check-equal? (rco-exp '(let ([x 2]) x)) '(let ([x 2]) x))
;(check-equal? (rco-exp (list 'read)) '(read))
;
(define make-list-from-vals (λ (a b) (list a b)))
(check-match 
  (call-with-values (λ () (rco-arg '(+ 2 2))) make-list-from-vals) 
  (list (? symbol?) (list (? symbol?) _)))

(displayln "tests finished")
