#!/usr/local/bin/racket
#lang racket

(provide uncover-live)


; **************************************************
; HELPERS
; **************************************************

; given a list of instructions
; and an initial live-after set (typically empty)
; returns the list of live-after Set (i.e. the Set datatype in Racket).
(define (get-live-after-sets instrs init-set)
  (match instrs
    [(not (? set? init-set))
     (error 'get-live-after-sets "bad init-set ~v" init-set)]
    [(? empty? instrs)
     ; there are no variables live after the last instruction
     ; so return the empty set
     (set)]
    [(list (? list? first-instr) rest-instrs ...)
     (define V (get-vars first-instr))
     (define R (get-read-vars first-instr))
     (define W (get-write-vars first-instr))
     "???"
     ;(define Lafter (set
     ;                set-subtract
     ;                set-union
     ]
    [_ (error 'get-live-after-sets "bad instrs ~v" instrs)]))

; compute the set of variables that appear in an argument (of an instruction)
; return a list of all variables
(define (get-vars instr)
  (error 'helper2 "not yet implemented"))

; compute the variables read by an instruction, which corresponds to the R(k)
; returns a list of symbols for variables that are read from
(define (get-read-vars instr)
  (error 'helper3 "not yet implemented"))

; compute the variables written by an instruction which corresponds to W(k)
; returns a list of symbols for variables that are written to
(define (get-write-vars instr)
  (error 'helper4 "not yet implemented"))

; **************************************************
; UNCOVER-LIVE
; **************************************************

; given an x86_0 program
; computes the live-after sets as described in Ch3 of Siek et. al.
; returns the same program 
; with the live-after sets as a list-of-lists-of-variables
; inside the "block" clause of the x86_0 grammar (Siek et. al. pg 24)
(define (uncover-live p)
  (error 'uncover-live "not yet implemented"))

; **************************************************
; TEST CASES (TODO: move into external file)
; **************************************************
