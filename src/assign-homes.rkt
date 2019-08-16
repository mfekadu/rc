#!/usr/local/bin/racket
#lang racket

(provide assign-homes)

(define WORD 8)
(define NEG_WORD -8)

; lookup symbol s in an association list assns
(define (lookup-offset assns s)
  (define filtered-assns (filter (λ (x) (equal? (car x) s)) assns))
  (cond [(= (length filtered-assns) 1) (cdar filtered-assns)]
        [else
         ; throw an error because there should never be variables that dont exist in the locals
         ; throw an error because there should be exactly 1 var that we find
         ; no duplicates allowed
         (error 'lookup-offset "~s not found or duplicates found" s)]))

; given an x86_0_prog
; return an x86_0_prog without vars and instead (deref rbp offset)
(define (assign-homes x86_0_prog)
  (match x86_0_prog
    [`(program ,locals (,label ,block))
     (match block
       [`(block ,info ,instrs)
        (define offsets (range NEG_WORD (* NEG_WORD (+ (length locals) 1)) NEG_WORD))
        (define assns (map cons locals offsets))
        `(program ,locals (,label (block ,info ,(assign-home-instrs instrs assns))))]
       [_ (error 'assign-homes "Bad block ~v " block)])]
    [_ (error 'assign-homes "bad program")]))

; given instructions and (var . offset) associations
; return a list of instructions where variables replaced with deref
(define (assign-home-instrs instrs assns)
  (for/list ([i instrs])
    (match i
      [`(,op (var ,v1) (var ,v2))
       `(,op (deref rbp ,(lookup-offset assns v1)) (deref rbp ,(lookup-offset assns v2)))]
      [`(,op ,anything (var ,v))
       `(,op ,anything (deref rbp ,(lookup-offset assns v)))]
      [`(,op (var ,v) ,anything)
       `(,op (deref rbp ,(lookup-offset assns v)) ,anything)]
      [`(,op (var ,v)) `(,op (deref rbp ,(lookup-offset assns v)))]
      [`(callq ,label) i]
      [`(jmp ,label) i]
      ; if we get an instruction that has no vars, then just return that instruction
      ; ie, movq (int 1) (reg rax) should return itself
      [`(,op ,any1 ,any2) i]
      [_ (error 'assign-home-instrs "wat? ~s" i)])))
