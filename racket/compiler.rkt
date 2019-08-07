#!/usr/bin/racket
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
              (rco
                (uniquify prog)))))))))


(define input1
  '(program () (+ 2 2)))

(displayln (compile input1))
