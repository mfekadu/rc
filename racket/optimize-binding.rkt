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
(define (optimize-binding let-expr unused-vars)
  (match let-expr
    [`(let [[,(? symbol? var) ,val]] ,body)
     #:when (is-let? body)
     (optimize-binding body unused-vars)]
    [_ let-expr]))



(check-equal? (optimize-binding '(let [[x1 20]] (let [[tmp (+ x1 22)]] (let [[y tmp]] y)))
                  (hash))
              '(let [[x1 20]] (let [[y (+ x1 22)]] y)))
(check-equal? (optimize-binding '(let [[x1 20]] (let [[tmp (+ x1 22)]] tmp))
                  (hash))
              '(let [[x1 20]] (+ x1 22)))