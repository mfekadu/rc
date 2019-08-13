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

(require racket/cmdline)

; more or less stolen from https://docs.racket-lang.org/reference/Command-Line_Parsing.html
(define file-to-compile
  (command-line
    #:program "compiler"
    #:args (filename)
    filename))

; convert string in file-to-compile into an sexp
(define input (read (open-input-file file-to-compile)))

(displayln (compile input))
