#!/usr/bin/racket
#lang racket
(require rackunit)
(require racket/contract)
(require "test-helpers.rkt") ; for check-fail and check-fail-with-name

(define (uniquify-exp alist)
  (lambda (e)
    (match e
      [(? symbol?) '()]
      [(? integer?) e]
      [`(let ([,x ,e]) ,body) '()]
      [`(,op ,es ...)
       `(,op ,@(for/list ([e es]) ((uniquify-exp alist) e)))]
      [_ (error "Malformed expression given to uniquify-exp: ~s" e)])))

(define (uniquify alist)
  (lambda (e)
    (match e
      [`(program ,info ,e)
       `(program ,info ,((uniquify-exp alist) e))]
      [_ (error "Malformed program given to uniquify: ~s" e)])))

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
