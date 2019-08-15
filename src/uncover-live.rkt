#!/usr/local/bin/racket
#lang racket

(provide uncover-live)


; **************************************************
; HELPERS
; **************************************************

; given a list of instructions
; and an initial live-after set (typically empty)
; returns the list of live-after sets.
(define (helper1 foo bar)
  (error 'helper1 "not yet implemented"))

; compute the set of variables that appear in an argument (of an instruction)
(define (helper2 foo bar)
  (error 'helper2 "not yet implemented"))0

; compute the variables read by an instruction, which corresponds to the R(k)
(define (helper3 foo bar)
  (error 'helper3 "not yet implemented"))

; compute the variables written by an instruction which corresponds to W(k)
(define (helper4 foo bar)
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
