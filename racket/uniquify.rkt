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
       `(,op ,@(for/list ([e es]) ((uniquify-exp alist) e)))])))

(define (uniquify alist)
  (lambda (e)
    (match e
      [`(program ,info ,e)
       `(program ,info ,((uniquify-exp alist) e))])))

(define uniquify-exp-func (uniquify-exp '()))

(define given1 '(+ 2 2))
(define expect1 '(+ 2 2))
(check-equal? (uniquify-exp-func given1) expect1)

(define given2 '(let ([x 2]) (+ 2 x)))
(define expect2 '(let ([x1 2]) (+ 2 x1)))
(check-equal? (uniquify-exp-func given2) expect2)

(define given3 '(let ([x 1]) (let ([x x]) (+ x x))))
(define expect3 '(let ([x1 1]) (let ([x2 x1]) (+ x2 x2))))
(check-equal? (uniquify-exp-func given3) expect3)
