#!/usr/bin/env racket
#lang racket

(provide allocate-registers)


; given an x86 program 
; return an x86 program with variables in registers or spilled on stack
(define (allocate-registers p)
  (error 'allocate-registers "not yet implemented"))

(require rackunit)

(define given1 '((movq (int 1) (var v))     ; 2
                 (movq (int 46) (var w))    ; 3
                 (movq (var v) (var x))     ; 4
                 (addq (int 7) (var x))     ; 5
                 (movq (var x) (var y))     ; 6
                 (addq (int 4) (var y))     ; 7
                 (movq (var x) (var z))     ; 8
                 (addq (var w) (var z))     ; 9
                 (movq (var y) (var t.1))   ; 10
                 (negq (var t.1))           ; 11
                 (movq (var z) (reg rax))   ; 12
                 (addq (var t.1) (reg rax)) ; 13
                 (jmp conclusion)))         ; 14

(define expect1 '((movq (int 1) (reg rcx))              ; 2
                  (movq (int 46) (deref rbp -16))       ; 3
                  (movq (reg rcx) (deref rbp -8))       ; 4
                  (addq (int 7) (deref rbp -8))         ; 5
                  (movq (deref rbp -8) (reg rcx))       ; 6
                  (addq (int 4) (reg rcx))              ; 7
                  (movq (deref rbp -8) (deref rbp -8))  ; 8 ; WHOA! WEIRD!
                  (addq (deref rbp -16) (deref rbp -8)) ; 9
                  (movq (reg rcx) (reg rcx))            ; 10 ; WHOA! WEIRD!
                  (negq (reg rcx))                      ; 11
                  (movq (deref rbp -8) (reg rax))       ; 12
                  (addq (reg rcx) (reg rax))            ; 13
                  (jmp conclusion)))                    ; 14

(check-equal? (allocate-registers given1) expect1)
