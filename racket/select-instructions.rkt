#!/usr/local/bin/racket
#lang racket
(require rackunit) ; for check-?? funcs
(require racket/exn) ; for exn->string
(require "test-helpers.rkt") ; for check-fail and check-fail-with-name

(provide select-instructions)

; Given a C0 arg (int or var), emit an x86_0 arg (int 2) or (reg register)
(define (handle-arg arg)
  (match arg
    ; TODO do fixnum everywhere!
    [(? fixnum? n) `(int ,n)]
    [(? symbol? s) `(var ,s)]
    [_ (if (number? arg)
           (error 'handle-arg "bad num: ~v" arg)
           (error 'handle-arg "bad arg: ~v" arg))]))

; Given a C0 expr [(+ 2 2), (- 3), (read)] and an output location (either (reg ...) or (var ...)
; return a list of instructions that puts the output of the expr into the specified location
(define (handle-expr expr output)
  ; TODO error check output to make sure it's either a var or reg
  (match expr
    [`(read)
      (if (eq? output '(reg rax))
        `((callq read_int))
        `((callq read_int) (movq (reg rax) ,output)))] 
    [`(+ ,args ..2) 
      `((movq ,(handle-arg (first args)) ,output)
        (addq ,(handle-arg (second args)) ,output))]
    [`(- ,arg)
      `((movq ,(handle-arg arg) ,output)
        (negq ,output))]
    ; if it isn't a list, assume it's a var, which only requires a mov instruction
    [(? (lambda (x) (not (list? x))) var) 
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
       `(program ,locals (,label ,(handle-tail tail)))]
      [_ (error 'select-instructions "bad c0-prog: ~v" c0-prog)])))


; ******************************
; TEST select-instructions
; ******************************
; a simple prog
(define given_prog_1 '(program () (start (return 42))))
(define expect_prog_1 '(program () (start ((movq (int 42) (reg rax)) (jmp conclusion)))))
(check-equal? (select-instructions given_prog_1) expect_prog_1)

; a prog with locals should remain
(define given_prog_2 '(program (x y z) (start (return 42))))
(define expect_prog_2 '(program (x y z) (start ((movq (int 42) (reg rax)) (jmp conclusion)))))
(check-equal? (select-instructions given_prog_2) expect_prog_2)

; a bad prog
(define given_bad_prog_1 '(program (start (return 42))))
(check-fail-with-name 'select-instructions select-instructions given_bad_prog_1)

; another bad prog
(define given_bad_prog_2 '(prog () (start (return 42))))
(check-fail-with-name 'select-instructions select-instructions given_bad_prog_2)

; one more bad prog
(define given_bad_prog_3 '(program () ()))
(check-fail-with-name 'select-instructions select-instructions given_bad_prog_3)

; test error message passing
(define given_bad_prog_4 `(program () (start (seq (assign 42 23) (return 0)))))
(check-fail-with-name 'select-instructions select-instructions given_bad_prog_4)


; ******************************
; TEST handle-tail
; ******************************
; a simple seq tail
(define given1 `(seq (assign x 2) (return x)))
(check-equal? (handle-tail given1)
                '((movq (int 2) (var x))
                  (movq (var x) (reg rax))
                  (jmp conclusion)))

; a bad tail
(check-fail (λ () (handle-tail 'foo)))


; a weirdly bad tail that assigns int to int
(define given_bad_assign_tail '(seq (assign 42 23) (return x)))
(check-fail (λ () (handle-tail given_bad_assign_tail)))
(check-fail-with-name 'handle-tail handle-tail given_bad_assign_tail)

; a simple return tail with var
(define given_return_with_var `(return x))
(check-equal? (handle-tail given_return_with_var)
                '((movq (var x) (reg rax))
                  (jmp conclusion)))

; a simple return tail with int
(define given_return_with_int `(return 42))
(check-equal? (handle-tail given_return_with_int)
                '((movq (int 42) (reg rax))
                  (jmp conclusion)))

; ******************************
; TEST handle-stmt
; ******************************
; an assign to plus
(define given2 '(assign x (+ 10 x)))
(check-equal? (handle-stmt given2) `((movq (int 10) (var x)) (addq (var x) (var x))))

; an assign to read
(define given3 '(assign x (read)))
(check-equal? (handle-stmt given3) `((callq read_int) (movq (reg rax) (var x))))

; an assign to negation
(define given4 '(assign x (- y)))
(check-equal? (handle-stmt given4) `((movq (var y) (var x)) (negq (var x))))

; an assign to fixnum
(define given5 '(assign x 5))
(check-equal? (handle-stmt given5) `((movq (int 5) (var x)))) ; should be a list of 1 instr

; an assign to fixnum out of fixnum bounds
(define BIG_NUM 9999999999999999999999999999999999)
(define given6 `(assign x ,BIG_NUM))
; make sure that the with-handler appends 'handle-stmt to the error
(check-fail-with-name 'handle-stmt handle-stmt given6)

; a bad stmt
(define given7 'give_bad_stmt)
(check-fail-with-name 'handle-stmt handle-stmt given7)

; ******************************
; TEST handle-tail
; ******************************
; a bad arg
(define given8 #t)
(check-fail-with-name 'handle-arg handle-arg given8)

; an good fixnum
(define given9 42)
(check-equal? (handle-arg given9) '(int 42))

; a bad fixnum
(define BIG_BIG_NUM 999999999999999999999999999999999999999999999)
(define given10 BIG_BIG_NUM)
; make sure 'handle-arg fails gracefully with its own name
(check-fail-with-name 'handle-arg handle-arg given10)

; a good symbol
(define given11 'variable)
(check-equal? (handle-arg given11) `(var ,given11))

; another bad arg
(define given12 handle-arg) ; the #<procedure:handle-arg> itself
(check-fail-with-name 'handle-arg handle-arg given12)

; test handle-tail sequence with return as tail
(define given13 '(seq (assign x 6) (return x)))
(check-equal? (handle-tail given13) '((movq (int 6) (var x)) (movq (var x) (reg rax)) (jmp conclusion)))

; test select-instructions whole program
(define given2-prog `(program (x) (start (seq (assign x (+ 10 x)) (return x)))))
(check-equal? (select-instructions given2-prog) `(program (x)
                                                  (start
                                                   ((movq (int 10) (var x))
                                                    (addq (var x) (var x))
                                                    (movq (var x) (reg rax))
                                                    (jmp conclusion)))))
