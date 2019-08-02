#!/usr/bin/racket
#lang racket
(require rackunit)
(require racket/contract)
(require "test-helpers.rkt") ; for check-fail and check-fail-with-name

; get the offsets
(define offsets (rest (map (λ (x) (* -8 x)) (range 6))))



; given an x86_0_prog
; return an x86_0_prog without vars and instead (deref rbp offset)
(define (assign-homes x86_0_prog) x86_0_prog)


(define given-prog '(program (tmp.1 tmp.2)
          (start ((movq (int 10) (var tmp.1))
                  (negq (var tmp.1))
                  (movq (var tmp.1) (var tmp.2))
                  (addq (int 52) (var tmp.2))
                  (movq (var tmp.2) (reg rax))
                  (jmp conclusion)))))


(check-equal? (assign-homes given-prog)
              '(program (tmp.1 tmp.2)
                        (start ((movq (int 10) (deref rbp -8))
                                (negq (deref rbp -8))
                                (movq (deref rbp -8) (deref rbp -16))
                                (addq (int 52) (deref rbp -16))
                                (movq (deref rbp -16) (reg rax))
                                (jmp conclusion)))))


              