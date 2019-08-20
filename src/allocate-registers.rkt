#!/usr/bin/env racket
#lang racket

(provide allocate-registers)


; **************************************************
; HELPERS
; **************************************************

; given an interference graph and a list of all variables in the program
; return a mapping of variables to their colors
; (as represented by numbers 0...N) where N is the number of variables
(define (color-graph graph locals)
  (error 'color-graph "not yet implemented"))

; given a mapping of variables to their colors
; and a list of register names e.g. '((reg rcx) (reg rbx))
; and the code??
; return the code but with the registers replacing the appropriate var
; and spill extra variables on the stack if need be
(define (assign-registers var_to_color reg_names code?)
  (error 'assign-registers "not yet implemented"))

; ari = allocate-registers-instructions
; given a list of x86 instruction
; return a list of x86 instruction with variables in registers or spilled on stack
(define (ari instrs)
  (match instrs
    [(? empty? instrs) instrs]
    [(list (? list? first-instr) rest-instrs ...) instrs]
    [_ (error 'ari "bad instrs ~v" instrs)]))


; **************************************************
; allocate-registers
; **************************************************

; given an x86 program 
; return an x86 program with variables in registers or spilled on stack
(define (allocate-registers p)
  (match p
    [`(program ,locals (,label (block ,info ,instrs)))
     `(program ,locals (,label (block ,info ,(ari instrs))))]
    [_ (error 'allocate-registers "Bad x86 program ~s " p)]))


; **************************************************
; TEST CASES
; **************************************************

(require rackunit)


; ==================================================
; TEST allocate-registers
; ==================================================
; test textbook program
(define given1-alloc-instrs '((movq (int 1) (var v))     ; 2
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
(define expect1-alloc-instrs '((movq (int 1) (reg rcx))              ; 2
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

(define given1-alloc-block `(block () ,given1-alloc-instrs))
(define given1-alloc-prog `(program () (start ,given1-alloc-block)))
(define expect1-alloc-block `(block () ,expect1-alloc-instrs))
(define expect1-alloc-prog `(program () (start ,expect1-alloc-block)))
(check-equal? (allocate-registers given1-alloc-prog) expect1-alloc-prog)


; ==================================================
; TEST color-graph
; ==================================================
; test ---
(check-true #t)

; test ---
(check-true #t)

; ==================================================
; TEST assign-registers
; ==================================================
; test ---
(check-true #t)

; test ---
(check-true #t)