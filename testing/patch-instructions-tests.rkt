#!/usr/bin/env racket
#lang racket
(require rackunit)
(require racket/contract)
(require "utilities.rkt") ; for check-fail and check-fail-with-name
(require "../src/patch-instructions.rkt")

; because every offset must only be a number that is a multiple of 8
(check-true (is-mult-8? 8))
(check-true (is-mult-8? 0))
(check-true (is-mult-8? -16))
(check-false (is-mult-8? -15))
(check-false (is-mult-8? 'foo))

; also because every offset must only be a negative number
(check-true (is-neg-mult-8? -8))
(check-false (is-neg-mult-8? 0))
(check-false (is-neg-mult-8? 16))
(check-false (is-neg-mult-8? -15))
(check-false (is-neg-mult-8? 'foo))

(define given_bad_offset '(addq (deref rbp v1) (deref rbp v2)))
(check-fail (λ () (replace-invalid-instrs given_bad_offset)))

(define given_bad_offset2 '(addq (deref rbp -8) (deref rbp 16)))
(check-fail (λ () (replace-invalid-instrs given_bad_offset2)))

(define given_bad_offset3 '(addq (int 2) (deref rbp v2)))
(check-fail (λ () (replace-invalid-instrs given_bad_offset3)))

(define given_bad_offset4 '(movq (deref rbp 8) (deref rbp -16)))
(check-fail (λ () (replace-invalid-instrs given_bad_offset4)))

(define given '(addq (deref rbp -8) (deref rbp -16)))
(define expect '((movq (deref rbp -8) (reg rax))
                 (addq (deref rbp -16) (reg rax))
                 (movq (reg rax) (deref rbp -16))))
(check-equal? (replace-invalid-instrs given) expect)

(define given2 '(addq (int 2) (deref rbp -8)))
(define expect2 `(,given2))
(check-equal? (replace-invalid-instrs given2) expect2)

(define given3 '(movq (deref rbp -8) (deref rbp -16)))
(define expect3 '((movq (deref rbp -8) (reg rax))
                 (movq (reg rax) (deref rbp -16))))
(check-equal? (replace-invalid-instrs given3) expect3)

(define given3prog `(program () ((block () (label start) ,given3))))
(define expect3prog `(program () ((block () (label start) ,@expect3))))
(check-equal? (patch-instructions given3prog) expect3prog)

(define given4-prog `(program () 
                        ((block 
                          ()
                          (label block176) (movq (int 0) (reg rax)) (jmp conclusion))
                        (block
                          ()
                          (label block178) (addq (deref rbp -8) (deref rbp -16)) (jmp block176))
                        (block
                          ()
                          (label block177) 
                          (cmpq (deref rbp -8) (deref rbp -16))
                          (set e (byte-reg al))
                          (movzbq (byte-reg al) (deref rbp -16)) ; needs to be changed!
                          (jmp block176))
                        (block
                          ()
                          (label start)
                          (movq (int 1) (reg rbx))
                          (movq (int 2) (reg rcx))
                          (movq (int 3) (reg rdx))
                          (cmpq (int 1) (int 1))
                          (jmp-if e block177)
                          (jmp block178)))))

(define given4-expect `(program () 
                        ((block 
                          ()
                          (label block176) (movq (int 0) (reg rax)) (jmp conclusion))
                        (block
                          ()
                          (label block178)
                          (movq (deref rbp -8) (reg rax))
                          (addq (deref rbp -16) (reg rax))
                          (movq (reg rax) (deref rbp -16))
                          (jmp block176))
                        (block
                          ()
                          (label block177) 
                          (movq (deref rbp -8) (reg rax))
                          (cmpq (deref rbp -16) (reg rax))
                          (set e (byte-reg al))
                          (movzbq (byte-reg al) (reg rax))
                          (movq (reg rax) (deref rbp -16))
                          (jmp block176))
                        (block
                          ()
                          (label start)
                          (movq (int 1) (reg rbx))
                          (movq (int 2) (reg rcx))
                          (movq (int 3) (reg rdx))
                          (movq (int 1) (reg rax))
                          (cmpq (int 1) (reg rax))
                          (jmp-if e block177)
                          (jmp block178)))))

(check-equal? (patch-instructions given4-prog) given4-expect)
(displayln "patch instructions tests pass")
