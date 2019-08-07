#!/usr/bin/racket
#lang racket
(require rackunit)
(require racket/contract)
(require "test-helpers.rkt") ; for check-fail and check-fail-with-name

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
    [`(program ,locals (,label ,instrs))
     (define offsets (range NEG_WORD (* NEG_WORD (+ (length locals) 1)) NEG_WORD))
     (define assns (map cons locals offsets))
     `(program ,locals (,label ,(assign-home-instrs instrs assns)))
     ]
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
      [_ (error 'assign-home-instrs "wat? ~s" i)])))


(define given-prog '(program (tmp.1 tmp.2)
          (start ((movq (int 10) (var tmp.1))
                  (negq (var tmp.1))
                  (movq (var tmp.1) (var tmp.2))
                  (addq (int 52) (var tmp.2))
                  (movq (var tmp.2) (reg rax))
                  (jmp conclusion)))))


(check-equal? (assign-homes given-prog)
              '(program (tmp.1 tmp.2)
                        (start ((movq (int 10) (deref rbp -8))
                                (negq (deref rbp -8))
                                (movq (deref rbp -8) (deref rbp -16))
                                (addq (int 52) (deref rbp -16))
                                (movq (deref rbp -16) (reg rax))
                                (jmp conclusion)))))


              
