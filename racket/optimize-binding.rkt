#!/usr/bin/racket
#lang racket
(require rackunit)

; given some expr,
; return #t if is-let else #f
(define (is-let? expr)
  (match expr
    [`(let [[,(? symbol? var) ,val]] ,body) #t]
    [_ #f]))
(check-true (is-let? '(let [[x 1]] x)))
(check-false (is-let? '(let [x 1] x)))
(check-false (is-let? 'x))
(check-false (is-let? '(let bind body)))
(check-false (is-let? 42))
(check-false (is-let? #t))
(check-false (is-let? #f))
(check-false (is-let? '(let [[1 x]] 42)));

; given a let-expr and a hashmap of unused-vars
; return and optimized version such that
; non-optimized is ......(let [[x1 20]] (let [[tmp (+ x1 22)]] (let [[y tmp]] y)))
; which becomes .........(let [[x1 20]] (let [[y (+ x1 22)]] y))
; OR non-optimized is ...(let [[x1 20]] (let [[tmp (+ x1 22)]] tmp))
; which becomes .........(let [[x1 20]] (+ x1 22))
; NOTE: only use `optimize-binding` after `uniquify`
;       because otherwise weird stuff could happen when we hash-set?
; NOTE; only use `optimize-binding` after remove-complex-opera
;       becuase it assumes that the ,val of each binding is already simple expr
;       also because it assumes the ,val is not a binding as well (i.e. simple expr)
(define (optimize-binding let-expr unused-vars)
  (match let-expr
    [`(let [[,(? symbol? var) ,val]] ,body)
     #:when (is-let? body)
     (define new-hash (hash-set unused-vars var val))
     (optimize-binding body new-hash)]
    [`(let [[,(? symbol? var) ,val]] ,body)
     ; if the above didn't match, then ,body is not let
     (define new-hash (hash-set unused-vars var val))
     (displayln new-hash)
     let-expr]
    ; anything else should stay the same
    [_ let-expr]))


; test anything else should stay the same
(check-equal? (optimize-binding '(+ 2 2) (hash)) '(+ 2 2))

(displayln '(let [[x1 20]] (let [[tmp (+ x1 22)]] (let [[y tmp]] y))))
(check-equal? (optimize-binding '(let [[x1 20]] (let [[tmp (+ x1 22)]] (let [[y tmp]] y)))
                  (hash))
              '(let [[x1 20]] (let [[y (+ x1 22)]] y)))

(displayln '(let [[x1 20]] (let [[tmp (+ x1 22)]] tmp)))
(check-equal? (optimize-binding '(let [[x1 20]] (let [[tmp (+ x1 22)]] tmp))
                  (hash))
              '(let [[x1 20]] (+ x1 22)))