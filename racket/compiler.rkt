#!/usr/local/bin/racket
#lang racket

(require "src/uniquify.rkt")
(require "src/rco.rkt")
(require "src/ec.rkt")
(require "src/patch-instructions.rkt")
(require "src/print-x86.rkt")
(require "src/select-instructions.rkt")
(require "src/uncover-locals.rkt")
(require "src/assign-homes.rkt")

(define (compile prog)
  (print-x86
    (patch-instructions
      (assign-homes
        (select-instructions
          (uncover-locals
            (explicate-control
              (rco-prog
                (uniquify prog)))))))))


(define input-broken
  '(program () (main 
                 (let ([x (+ 3 (+ 1 2))]) (+ x (- 1))))))

(define input2
  '(program () (main 
                 (let ([x (+ 3 (+ 1 2))]) x))))


; expect: 
; .global main
; main:
;   movq $2, %rax
;   addq $2, %rax
;   retq
;(displayln (explicate-control (rco-prog (uniquify input1))))
(displayln (compile input-broken))

;(define uniquified (uniquify input2))
;(define rcod (rco-prog uniquified))
;(define ecd (explicate-control rcod))
;(define locals-uncovered (uncover-locals ecd))
;(define instr-selected (select-instructions locals-uncovered))
;(define homes-assigned (assign-homes instr-selected))
;(displayln homes-assigned)
;(define instructions-patched (patch-instructions homes-assigned))
;(displayln instructions-patched)
