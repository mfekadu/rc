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
     ; instrs is a list of instructions
      `(program ,locals (,label ,(rec-replace-invalid-instrs instrs)))]
    [_ (error "Malformed input program to patch-instructions ~s " x86-prog)]))

(define (rec-replace-invalid-instrs instrs)
  (cond [(empty? instrs) (error 'rec-replace-invalid-instrs "bad empty instr")]
        [else (append (replace-invalid-instrs (first instrs))
                      (rec-replace-invalid-instrs (rest instrs)))]))

(define (replace-invalid-instrs instrs)
  (match instrs
    [`(movq (deref rbp ,offset1) (deref rbp ,offset2))
     `((movq (deref rbp ,offset1) (reg rax))
       (movq (reg rax) (deref rbp ,offset2)))]
    [`(,op (deref rbp ,offset1) (deref rbp ,offset2))
     ;LIST OF INSTRS???
     `((movq (deref rbp ,offset1) (reg rax))
      (addq (deref rbp ,offset2) (reg rax))
      (movq (reg rax) (deref rbp ,offset1)))
     ]
    [_ `(,instrs)]))


(define given '(addq (deref rbp v1) (deref rbp v2)))
(define expect '((movq (deref rbp v1) (reg rax))
                 (addq (deref rbp v2) (reg rax))
                 (movq (reg rax) (deref rbp v1))))
(check-equal? (replace-invalid-instrs given) expect)

(define given2 '(addq (int 2) (deref rbp v2)))
(define expect2 `(,given2))
(check-equal? (replace-invalid-instrs given2) expect2)

(define given3 '(movq (deref rbp v1) (deref rbp v2)))
(define expect3 '((movq (deref rbp v1) (reg rax))
                 (movq (reg rax) (deref rbp v2))))
(check-equal? (replace-invalid-instrs given3) expect3)