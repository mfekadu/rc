#!/usr/bin/env racket
#lang racket
(require rackunit) ; for check-?? funcs
(require racket/exn) ; for exn->string

(provide select-instructions)
(provide handle-arg)
(provide handle-stmt)
(provide handle-tail)
(provide handle-expr)

; Given a C0 arg (int or var), emit an x86_0 arg (int 2) or (reg register)
(define (handle-arg arg)
  (match arg
    ; TODO do fixnum everywhere!
    [(? fixnum? n) `(int ,n)]
    [(? symbol? s) `(var ,s)]
    [_ (if (number? arg)
           (error 'handle-arg "bad num: ~v" arg)
           (error 'handle-arg "bad arg: ~v" arg))]))

; Given a C0 expr [(+ 2 2), (- 3), (read)]
; and an output location (either (reg ...) or (var ...)
; return a list of instructions that puts the output of the expr into the specified location
(define (handle-expr expr output)
  ; matching on the cons of the params allows to making sure that the output is either reg or var
  (match (cons expr output)
    [(cons `(read) (list (or 'reg 'var) _))
      (if (equal? output '(reg rax))
        `((callq read_int))
        `((callq read_int) (movq (reg rax) ,output)))]
    [(cons `(+ ,args ..2) (list (or 'reg 'var) _))
      `((movq ,(handle-arg (first args)) ,output)
        (addq ,(handle-arg (second args)) ,output))]
    [(cons `(- ,arg) (list (or 'reg 'var) _))
      `((movq ,(handle-arg arg) ,output)
        (negq ,output))]
    ; if it isn't a list, assume it's a var, which only requires a mov instruction
    [(cons (? (lambda (x) (not (list? x))) var) (list (or 'reg 'var) _))
     `((movq ,(handle-arg var) ,output))]
    [_ (error "handle-expr failed on expression")]))

; Given a C0 statement (assign), emit an x86_0 instruction (movq or addq or negq)
(define (handle-stmt stmt)
  (with-handlers ([exn:fail? (λ (exn) (error 'handle-stmt (exn->string exn)))])
    (match stmt
      ; if it's third element in stmt is a list, it must be an expr
      [`(assign ,(? symbol? lhs) ,expr)
        (define output (handle-arg lhs))
        (handle-expr expr output)]
      [_ (error 'handle-stmt "bad stmt: ~v" stmt)])))

; Given a C0 tail (sequence or return), call handle-stmt on statement and itself(?) on tail
(define (handle-tail tail)
  (with-handlers ([exn:fail? (λ (exn) (error 'handle-tail (exn->string exn)))])
    (match tail
      [`(return ,expr)
       (append (handle-expr expr '(reg rax)) '((jmp conclusion)))]
      [`(seq ,stmt ,new-tail)
       ; handle the stmt and recursively call handle-tail on the tail
       (define new-stmt-instr (handle-stmt stmt))
       (define new-tail-instr (handle-tail new-tail))
       (append new-stmt-instr new-tail-instr)]
      [_ (error 'handle-tail "bad tail:" tail)])))

;(define given_ret_plus_2_2  '(return (+ 2 2)) )
;(define expect_ret_plus_2_2 '(movq )  )
;(check-equal? (handle-tail given_ret_plus_2_2) expect_ret_plus_2_2

; given a C0 program, return a pseudo-x86_0 program
(define (select-instructions c0-prog)
  (with-handlers ([exn:fail? (λ (exn) (error 'select-instructions (exn->string exn)))])
    (match c0-prog
      [`(program ,locals (,label ,tail))
      ; TODO: consider (.globl ,label)
       `(program ,locals (,label (block () ,(handle-tail tail))))]
      [_ (error 'select-instructions "bad c0-prog: ~v" c0-prog)])))