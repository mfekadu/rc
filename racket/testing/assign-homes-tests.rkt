#!/usr/local/bin/racket
#lang racket
(require rackunit)
(require racket/contract)
(require "test-helpers.rkt") ; for check-fail and check-fail-with-name
(require "../src/assign-homes.rkt")

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


(displayln "assign homes tests passed")
