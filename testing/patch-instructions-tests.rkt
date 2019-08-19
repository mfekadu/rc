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

(displayln "patch instructions tests pass")
