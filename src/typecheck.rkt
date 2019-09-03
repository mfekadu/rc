#!/usr/bin/env racket
#lang racket

(provide typecheck-exp)


; typecheck-exp returns the type of an expression
; it will error if the expression mismatches types
(define (typecheck-exp env e)
  (match e
    [(? fixnum?) 'Integer]
    [(? boolean?) 'Boolean]
    [(? symbol? x) (dict-ref env x)]
    ['(read) 'Integer]
    [`(let ([,var ,val]) ,body)
     (define var-type (typecheck-exp env val))
     (define new-env (cons (cons var var-type) env))
     (typecheck-exp new-env body)]

    [`(+ ,e1 ,e2)
      (define t1 (typecheck-exp env e1))
      (define t2 (typecheck-exp env e2))
      (cond
        [(and (eq? t1 'Integer) (eq? t2 'Integer)) 'Integer]
        [else (error "typecheck: Type mismatch on expr: ~v" e)])]

    ; general case - handle boolean ops
    ; TODO how to explicitly match (eq?, >, <, >=, <=, =) instead without duplicating code
    [`(,op ,e1 ,e2)
      (define t1 (typecheck-exp env e1))
      (define t2 (typecheck-exp env e2))
      (cond
        ; TODO is this the correct way to compare symbols?
        ; return a boolean
        [(eq? t1 t2) 'Boolean]
        [else (error "typecheck: Type mismatch on expr: ~v" e)])] 
    [_ (error "typecheck: Unrecognized expr ~v" e)]))



; top level typecheck function
; calls typecheck-exp on the body of the program
; returns its input if there was no error
(define (typecheck env program)
  (match program
    [`(program ,info ,body)

     ; find the type of the body, but don't do anything with it
     ; it will error if there's a type mismatch somewhere 
     (define type (typecheck-exp '() body))

     ; return the input program if no type errors
     `(program ,info ,body)]

    [_ (error "Malformed program input to typecheck")]))
