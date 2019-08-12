#!/usr/local/bin/racket
#lang racket
(require rackunit)
(require racket/contract)
(require "test-helpers.rkt") ; for check-fail and check-fail-with-name
(require "../src/print-x86.rkt")

; TEST print-x86
(check-true (string? (print-x86 '())))
(define instr '((addq (int 2) (deref rbp -8))))
(define expect (format "      .global ~s\n~s:\n      movq %rsp, %rbp\n      addq $2, -8(%rbp)\n" (string->symbol MAIN) (string->symbol MAIN)))
(check-equal? (print-x86 `(program () (start ,instr))) expect)

; TEST print-x86-arg
(check-equal? (print-x86-arg '(int 42)) "$42")
(check-equal? (print-x86-arg '(reg rsp)) "%rsp")
(check-equal? (print-x86-arg '(reg r12)) "%r12")
(check-equal? (print-x86-arg '(deref rbp -8)) "-8(%rbp)")
(check-fail (λ () (print-x86-arg 'foobar)))
(check-fail (λ () (print-x86-arg 'foobar)))

; TEST print-x86-instr
(check-fail (λ () (print-x86-instr 'x)))
(check-equal? (print-x86-instr '(addq (int 42) (reg rax))) "addq $42, %rax")

(displayln "print-x86 tests finished")
