#!/usr/local/bin/racket
#lang racket
(require rackunit)
(require racket/contract)
(require "utilities.rkt") ; for check-fail and check-fail-with-name
(require "../src/assign-homes.rkt")

(define given-prog '(program (tmp.1 tmp.2)
          (start (block () ((movq (int 10) (var tmp.1))
                  (negq (var tmp.1))
                  (movq (var tmp.1) (var tmp.2))
                  (addq (int 52) (var tmp.2))
                  (movq (var tmp.2) (reg rax))
                  (jmp conclusion))))))


(check-equal? (assign-homes given-prog)
              '(program (tmp.1 tmp.2)
                        (start (block () ((movq (int 10) (deref rbp -8))
                                (negq (deref rbp -8))
                                (movq (deref rbp -8) (deref rbp -16))
                                (addq (int 52) (deref rbp -16))
                                (movq (deref rbp -16) (reg rax))
                                (jmp conclusion))))))

; test case for instructions that do not use any vars
(define given2 '(program () (main (block () ((movq (int 42) (reg rax)))))))
(check-equal? (assign-homes given2) given2)

(displayln "assign homes tests finished")
