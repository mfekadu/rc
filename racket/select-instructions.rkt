#!/usr/bin/racket
#lang racket
(require rackunit)

(define (select-instructions c0-prog)
  (match c0-prog
    [`(program ,locals (,label ,tail)) 
      (displayln tail)
      `((.globl ,label) ,locals (,label ,(handle-tail tail)))]
    [_ (error "oh no")])) 

; Given a C0 tail (sequence or return), call handle-stmt on statement and itself(?) on tail
(define (handle-tail tail)
  (match tail
    [`(return ,expr) 
      ; treat return as an assign to rax
      ; then do a jump to the conclusion
      ; .... where is conclusion? is it labeled?
      `(movq ,(handle-arg expr) (reg rax))]
    [`(seq ,stmt ,new-tail) 
      (displayln "here")
      ; handle the stmt and recursively call handle-tail on the tail
      (define new-stmt-instr (handle-stmt stmt))
      (define new-tail-instr (handle-tail new-tail))
      (append new-stmt-instr new-tail-instr)]
    [_ (error "uhhhhhhhhhhhhhhhhhhhhhh")]))

; Given a C0 arg (int or var), emit an x86_0 arg (int 2) or (reg register)
(define (handle-arg arg)
  (match arg
    ; TODO do fixnum everywhere!
    [(? fixnum? n) `(int ,n)]
    [(? symbol? s) `(var ,s)]))

; Given a C0 statement (assign), emit an x86_0 instruction (movq or addq or negq)
(define (handle-stmt stmt)
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
    [_ (error "plsno")]))

(define given1 '((movq (int 32) (var x)) (addq (int 32) (var x))))

(define given2 '(assign x (+ 10 x)))
(check-equal? (handle-stmt given2) `((movq (int 10) (var x)) (addq (var x) (var x))))

(define given3 '(assign x (read)))
(check-equal? (handle-stmt given3) `((callq read_int) (movq (reg rax) (var x))))

(define given4 '(assign x (- y)))
(check-equal? (handle-stmt given4) `((movq (var y) (var x)) (negq (var x))))

(define given5 '(assign x 5))
(check-equal? (handle-stmt given5) `(movq (int 5) (var x)))

; testing handle-tail
(define given6 '(seq (assign x 6) (return x)))
(check-equal? (handle-tail given6) '((movq (int 6) (var x)) (movq (var x) (reg rax))))

; tests for handle-tail
;(define given2-prog `(program (x) (start (seq (assign x (+ 10 x)) (return x)))))
;(check-equal? (select-instructions given2-prog) `((.globl start) (x)
;                                                  (start
;                                                    (movq (int 10) (var x))
;                                                    (addq (var x) (var x))
;                                                    (mov (var x) (reg rax)))))
