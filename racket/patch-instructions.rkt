#!/usr/bin/racket
#lang racket
(require rackunit)
(require racket/contract)
(require "test-helpers.rkt") ; for check-fail and check-fail-with-name

; Given an x86 program, break up all instructions of the form
;               op (deref rbp v1) (deref rbp v2)
; into 
;               mov (deref rbp v1) (reg rax)
;               op (deref rbp v2) (reg rax)
;               mov (reg rax) (deref rbp v1)
;
; TODO There's a potential OPTIMIZATION here, e.g.:
;     addq (deref rbp -8) (deref rbp -16)
;     addq (deref rbp -8) (deref rbp -24)
; Would become (unoptimized):
;     mov (deref rbp -8) (reg rax)
;     addq (deref rbp -16) (reg rax)
;     mov (reg rax) (deref rbp -8)
;     mov (deref rbp -8) (reg rax)
;     addq (deref rbp -24) (reg rax)
;     mov (reg rax) (deref rbp -8)
(define (patch-instructions x86-prog)
  (match x86-prog 
    [`(program ,locals (,label ,instrs))
      `(program ,locals (,label ,(replace-invalid-instrs instrs)))]
    [_ (error "Malformed input program to patch-instructions ~s " x86-prog)]))

(define (replace-invalid-instrs instrs)
  (match instrs))
