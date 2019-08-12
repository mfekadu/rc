#!/usr/local/bin/racket
#lang racket
(require rackunit) ; for check-?? funcs
(require racket/exn) ; for exn->string

(provide select-instructions)
(provide handle-arg)
(provide handle-stmt)
(provide handle-tail)

; Given a C0 arg (int or var), emit an x86_0 arg (int 2) or (reg register)
(define (handle-arg arg)
  (match arg
    ; TODO do fixnum everywhere!
    [(? fixnum? n) `(int ,n)]
    [(? symbol? s) `(var ,s)]
    [_ (if (number? arg)
           (error 'handle-arg "bad num: ~v" arg)
           (error 'handle-arg "bad arg: ~v" arg))]))

; Given a C0 statement (assign), emit an x86_0 instruction (movq or addq or negq)
(define (handle-stmt stmt)
  (with-handlers ([exn:fail? (λ (exn) (error 'handle-stmt (exn->string exn)))])
    (match stmt
      [`(assign ,(? symbol? lhs) (read))
       `((callq read_int)
         (movq (reg rax) (var ,lhs)))]
      [`(assign ,(? symbol? lhs) (+ ,args ..2))
       (define x86-var (handle-arg lhs))
       `((movq ,(handle-arg (first args)) ,x86-var)
         (addq ,(handle-arg (second args)) ,x86-var))]
      [`(assign ,(? symbol? lhs) (- ,arg))
       (define x86-var (handle-arg lhs))
       `((movq ,(handle-arg arg) ,x86-var)
         (negq ,x86-var))]
      [`(assign ,(? symbol? lhs) ,val) `((movq ,(handle-arg val) ,(handle-arg lhs)))]
      [_ (error 'handle-stmt "bad stmt: ~v" stmt)])))


; Given a C0 tail (sequence or return), call handle-stmt on statement and itself(?) on tail
(define (handle-tail tail)
  (with-handlers ([exn:fail? (λ (exn) (error 'handle-tail (exn->string exn)))])
    (match tail
      ;[`(return (+ ,a1 ,a2))
      ;  
      ;  ]
      [`(return ,expr)
       ; see pg 22 of textbook
       `((movq ,(handle-arg expr) (reg rax))
         (jmp conclusion))]
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
       `(program ,locals (,label ,(handle-tail tail)))]
      [_ (error 'select-instructions "bad c0-prog: ~v" c0-prog)])))



