#!/usr/local/bin/racket
#lang racket

(require "uniquify.rkt")
(require "rco.rkt")
(require "ec.rkt")
(require "patch-instructions.rkt")
(require "print-x86.rkt")
(require "select-instructions.rkt")
(require "uncover-locals.rkt")
(require "assign-homes.rkt")

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
  '(program () (let ([x (+ 3 (+ 1 2))]) (+ x (- 1)))))

(define input2
  '(program () (let ([x (+ 3 (+ 1 2))]) x)))

(define input3 '(program () (main (+ 2 2))))

; expect: 
; .global main
; main:
;   movq $2, %rax
;   addq $2, %rax
;   retq
;(displayln (explicate-control (rco-prog (uniquify input1))))
;(displayln (select-instructions (explicate-control (rco-prog input2))))
(displayln (compile input3))

;(define uniquified (uniquify input2))
;(define rcod (rco-prog uniquified))
;(define ecd (explicate-control rcod))
;(define locals-uncovered (uncover-locals ecd))
;(define instr-selected (select-instructions locals-uncovered))
;(define homes-assigned (assign-homes instr-selected))
;(displayln homes-assigned)
;(define instructions-patched (patch-instructions homes-assigned))
;(displayln instructions-patched)
