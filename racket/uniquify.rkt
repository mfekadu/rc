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
      ['(let ([,x ,e]) ,body) '()]
      ['(,op ,es ...)
       '(,op ,@(for/list ([e es]) ((uniquify-exp alist) e)))])))

(define (uniquify alist)
  (lambda (e)
    (match e
      ['(program ,info ,e)
       '(program ,info ,((uniquify-exp alist) e))])))
