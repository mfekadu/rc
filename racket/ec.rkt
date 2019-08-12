#!/usr/local/bin/racket
#lang racket

(provide explicate-control)
(provide ec-tail)
(provide ec-assign)

; function to break off the program part of the C0 syntax
(define (explicate-control prog) 
  (match prog
    [`(program ,locals (,label ,expr)) 
      `(program ,locals (,label ,(ec-tail expr)))]
    [_ (error "malformed program input to ec-prog")]))

(define (ec-tail e)
  (match e
    ; when given something simple, ec-tail makes returns
    [(? symbol?)               `(return ,e)]
    [(? integer?)              `(return ,e)]
    ; when an expr with assignments, it relies on it's friend ec-assign
    [`(let ([,var ,val]) ,body) (ec-assign val var (ec-tail body))]
    ; operations are just operations
    ; operation arm at bottom to allow for matching let first
    [`(,op ,args ...) `(return ,e)]))

(define (ec-assign val var tail)
  (match val
    ; when given simple cases, make a C0 that looks like `var = val; `tail;``
    [(? symbol? s)    `(seq (assign ,var ,s) ,tail)]
    [(? integer?)     `(seq (assign ,var ,val) ,tail)]
    [`(read)          `(seq (assign ,var ,val) ,tail)]
    ; when given 
    [`(let ([,new_var ,val]) ,body)
     (define new_tail (ec-assign body var tail)) 
     (ec-assign val new_var new_tail)]
    ; operations are similar to the atomic cases
    [`(,op ,args ...) `(seq (assign ,var ,val) ,tail)]))
