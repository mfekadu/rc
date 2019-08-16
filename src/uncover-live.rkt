#!/usr/local/bin/racket
#lang racket

(provide uncover-live get-vars get-read-vars get-write-vars get-live-after-sets)


; **************************************************
; HELPERS
; **************************************************

; compute the set of variables that appear in an argument (of an instruction)
; return a Set of all variables
(define (get-vars instr)
  (match instr
    ; e.g. (addq (var x) (var y))
    [`(,(? symbol? op) (var ,src) (var ,dest)) (set src dest)]
    ; match a var anywhere in the args while ensuring a symbol for opcode
    [(list-rest (? symbol? op) (list-no-order `(var ,v) arg)) (set v)]
    ; match e.g. (negq (var t)) 
    [`(,(? symbol? op) (var ,v)) (set v)]
    ; match e.g. (jmp conclusion)
    [`(,(? symbol? op) ,arg) (set)]
    [`(,(? symbol? op) ,arg1 ,arg2) (set)] ; return empty set if no var
    [_ (error 'get-vars "bad instr: ~v" instr)]))

; compute the variables read by an instruction, which corresponds to the R(k)
; returns a Set of symbols for variables that are read from
(define (get-read-vars instr)
  (match instr
    ; e.g. (negq (var t))
    [`(,(? symbol? op) (var ,v)) (set v)]
    ; e.g. (jmp conclusion)
    [`(,(? symbol? op) ,arg) (set)]
    ; e.g. (addq (var x) (var y)) ; addq is special case, reads both
    [`(addq (var ,src) (var ,dest)) (set src dest)]
    ; e.g. (addq (var t) (reg rax)) ; reads reg too but that's not var
    ; e.g. (addq (int 42) (var y))
    [(list-rest 'addq (list-no-order `(var ,v) arg2)) (set v)]
    ; e.g. (movq (var x) (var y))
    [`(,op (var ,src) (var ,dest)) (set src)]
    ; e.g. (movq (var t) (reg rax))
    [`(,(? symbol? op) (var ,src) ,arg2) (set src)]
    ; e.g. (movq (int 42) (reg rax))
    [`(,(? symbol? op) ,arg1 ,arg2) (set)] ; return empty set if no var
    [_ (error 'get-read-vars "bad instr: ~v" instr)]))

; compute the variables written by an instruction which corresponds to W(k)
; returns a Set of symbols for variables that are written to
(define (get-write-vars instr)
  (match instr
    ; e.g. (negq (var t))
    [`(,(? symbol? op) (var ,v)) (set v)]
    ; e.g. (jmp conclusion)
    [`(,(? symbol? op) ,arg) (set)]
    ; e.g. (addq (var x) (var y))
    ; e.g. (movq (var x) (var y))
    ; e.g. (addq (reg rax) (var t))
    ; e.g. (movq (reg rax) (var t))
    ; e.g. (addq (int 42) (var y))
    ; convension is write only to dest register
    [`(,(? symbol? op) ,arg1 (var ,dest)) (set dest)]
    ; e.g. (addq (int 42) (reg rax))
    ; e.g. (movq (int 42) (reg rax)) ; write reg too but not var
    [`(,(? symbol? op) ,arg1 ,arg2) (set)] ; return empty set if no var
    [_ (error 'get-write-vars "bad instr: ~v" instr)]))


; given a list of instructions
; and an initial live-after set (typically empty)
; returns the list of live-after Set (i.e. the Set datatype in Racket).
(define (get-live-after-sets instrs init-set)
  (cond
    [(not (set? init-set))
     (error 'get-live-after-sets "bad init-set ~v" init-set)])
  (match instrs
    [(? empty? instrs)
     ; there are no variables live after the last instruction
     ; so return a list containing the empty set
     (list (set))]
    [(list (? list? first-instr) rest-instrs ...)
     ; recursively get the Live_after set from bottom to top
     ; at bottom L_after is (list (set))
     (define L_after (get-live-after-sets rest-instrs init-set))
     (define V (get-vars first-instr))
     (define R (get-read-vars first-instr))
     (define W (get-write-vars first-instr))
     (define L_before (set-union (set-subtract (first L_after) W) R))
     ; now return the combined list of sets
     (cons L_before L_after)]
    [_ (error 'get-live-after-sets "bad instrs ~v" instrs)]))

; **************************************************
; UNCOVER-LIVE
; **************************************************

; given an x86_0 program
; computes the live-after sets as described in Ch3 of Siek et. al.
; returns the same program 
; with the live-after sets as a list-of-lists-of-variables
; inside the "block" clause of the x86_0 grammar (Siek et. al. pg 24)
(define (uncover-live p)
  (match p
    [`(program ,locals (,label ,block))
     (match block
       [`(block ,info ,instrs)
        (define LAS (get-live-after-sets instrs (set)))
        `(program ,locals (,label (block ,LAS ,instrs)))]
       [_ (error 'uncover-live "bad block ~v" block)])]
    [_ (error 'uncover-live "Bad x86_0 program ~s " p)]))