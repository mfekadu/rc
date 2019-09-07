#!/usr/bin/env racket
#lang racket

(provide explicate-control)
(provide ec-tail)
(provide ec-assign)
(provide CFG-to-list)

; global Control Flow Graph
; key = label, value = C1 tail 
(define CFG (make-hash))

(define (CFG-to-list)
   (map (lambda (x) `(,(car x) ,(cdr x))) (hash->list CFG)))

; explicate-control
; given an R1 program output C0
(define (explicate-control prog) 
  (hash-clear! CFG)
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
    [(? symbol?)  `(return ,e)]
    [(? integer?) `(return ,e)]
    [(? boolean?) `(return ,e)]
    ['(read)      `(return ,e)]
    [`(if ,cnd ,thn ,els) 
      (define thn-tail (ec-tail thn))
      (define els-tail (ec-tail els))
      (ec-pred cnd thn-tail els-tail)]
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
    [(? boolean?)     `(seq (assign ,var ,val) ,tail)]
    [`(read)          `(seq (assign ,var ,val) ,tail)]

    ; need to handle a (let ([x (if ...)])) case
    ; if statement is in assignment position
    [`(if ,cnd ,thn ,els)
      ; After either the then or els case, the code will need to go to the tail
      ; So we add it to the CFG and append a (goto tail-label) to both the thn and els
      ; case. This prevents some code duplication.
      (define tail-label (gensym 'block))
      (hash-set! CFG tail-label tail)

      ; create both branches where we assign val to the result of both of them 
      (define thn-block (ec-assign thn var `(goto ,tail-label)))
      (define els-block (ec-assign els var `(goto ,tail-label)))
      
      ; call ec-pred on the cnd with the thn and els blocks
      (ec-pred cnd thn-block els-block)]

    ; wait a sec we already handle when lets are in assignment position???
    [`(let ([,new_var ,new_val]) ,body)
      ; assign the result of the body to the var passed in 
      ; this is the tail that occurs AFTER assigning new_val to new_var 
      (define new_tail (ec-assign body var tail))

      ; create a tail where new_val gets assigned to new_var that then goes to
      ; new_tail after the assignment
      (ec-assign new_val new_var new_tail)]
    ; operations are similar to the atomic cases
    [`(,op ,args ...) `(seq (assign ,var ,val) ,tail)]))

(define (ec-pred cnd thn-tail els-tail)
  ; create labels
  (define thn-label (gensym 'block))
  (define els-label (gensym 'block))

  ; update global CFG
  (hash-set! CFG thn-label thn-tail)
  (hash-set! CFG els-label els-tail)
  
  (match cnd
    ; base case - just have a symbol or #t or #f
    ; RCO always replaces (?)
    [(or (? symbol?) #t #f) `(if (eq? ,cnd #t) (goto ,thn-label) (goto ,els-label))]

    [`(let ([,var ,val]) ,body) (error "let nested in if cnd shouldn't happen")]
    ; uh oh we got an if nested inside a pred
    [`(if ,cnd2 ,thn2 ,els2) (error "nested if not handled")]
    [_ (error "Unhandled condition in if statement: " cnd)]))
 
