#!/usr/bin/env racket
#lang racket

(provide explicate-control)
(provide ec-tail)
(provide ec-assign)

; global Control Flow Graph
; key = label, value = C1 tail 
(define CFG (make-hash))

(define (CFG-to-list)
   (map (lambda (x) `(,(car x) ,(cdr x))) (hash->list CFG)))

; explicate-control
; given an R1 program output C0
(define (explicate-control prog)
  (match prog
    [`(program ,locals (,label ,(? list? e)))
     ; Strictly follow the R1 grammar for now because '(start (+ 2 2)) is not an expr
     ; Feel free to refactor for R2.
     (error 'explicate-control "Malformed expr in program: ~s" prog)]
    [`(program ,locals ,expr)
      (define start-code (ec-tail expr))
      ; put start-code into CFG with key start
      (hash-set! CFG 'start start-code)
      `(program ,locals ,(CFG-to-list))]
    [_ (error 'explicate-control "Malformed program: ~s" prog)]))

; ec-tail
; given an R1 expr output a C0 tail
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

; given a R1 binding expr, output a C0 assign
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

