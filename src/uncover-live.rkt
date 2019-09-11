#!/usr/bin/env racket
#lang racket
(require graph)
(require "utilities.rkt")

(provide uncover-live get-vars get-read-vars get-write-vars
         get-live-after-sets get-LAS-from-blocks)

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
    ; e.g. (label start)
    [`(label _) (set)]
    ; e.g. (jmp conclusion)
    [`(jmp _) (set)]
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
    ; e.g. (label start)
    [`(label _) (set)]
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
     (list init-set)]
    [(list (? list? first-instr) rest-instrs ...)
     ; recursively get the Live_after set from bottom to top
     ; at bottom L_after is (list (set))
     ; So. L_after means after `rest-instrs`
     (define L_after (get-live-after-sets rest-instrs init-set))
     ; Now calculate the L_before `rest-instrs` aka L_after `first-instr`
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

; helper to handle multiple-blocks because get-live-after-sets handles single-block
; given hash-of-blocks and graph (DAG)
; return live-after-set???
(define (get-LAS-from-blocks H g)
  (define vs (remove 'conclusion (reverse (tsort g))))
  (define edges (get-edges g))
  (define get_prev_LAS
      (λ (x)
        (with-handlers ([exn:fail? (λ (exn) (set))])
          ; try to get the previous "LAS" within a block
          ; if error for any reason...
          ; empty set returned via with-handlers
          (first (second (hash-ref H x))))))
  (define make_block_with_LAS_from_original_in_hash
    (λ (L x)
      (match (hash-ref H x)
        [`(block ,info ,instrs ...) `(block ,L ,@instrs)]
        [else (error 'lolwut "unexpected?? ~v" else)])))
  (define set_LAS_in_hash
    (λ (L x)
      (define new_block (make_block_with_LAS_from_original_in_hash L x))
      (hash-set! H x new_block)))
  (for/list ([v vs])
    (define b (hash-ref H v))
    (define neighbors (get-neighbors g v))
    
    (define init_set
      (foldl set-union (set) (map get_prev_LAS neighbors)))

    (define L (get-live-after-sets (drop b 2) init_set))
    (set_LAS_in_hash L v)
    (hash-ref H v)))

;
(define (to-string x)
  (match x
    [(? symbol?) (symbol->string x)]
    [_ (format "~s" x)]))

;
(define PLACES-WE-FIND-BLOCK-SYMBOLS '(label jmp jmp-if callq))

; its an ugly function
; TODO: test this for real
(define (get-adj-list block)
  (match block
    [`(block () ,instrs ...)
     (filter-map
      (λ (x) (and (member (car x) PLACES-WE-FIND-BLOCK-SYMBOLS) (last x)))
      instrs)]
    [_ (error 'get-adj-list "bad block construct ~v" block)]))


; given an x86_0 program
; computes the live-after sets as described in Ch3 of Siek et. al.
; returns the same program 
; with the live-after sets as a list-of-lists-of-variables
; inside the "block" clause of the x86_0 grammar (Siek et. al. pg 24)
(define (uncover-live p)
  (match p
    [`(program ,locals ,(? blocks? blocks))
     #:when (not (empty? blocks))
     (define dag_adj_matrix (for/list ([b blocks]) (get-adj-list b)))
     (define g (unweighted-graph/adj dag_adj_matrix))

     ; do the magical mutable magic in O(n)
     (define h (make-hash))
     (for ([b blocks])
       (hash-set! h (second (third b)) b))
     (define updated_blocks (get-LAS-from-blocks h g))
     `(program ,locals ,updated_blocks)]
    [_ (error 'uncover-live "Bad x86 program ~s " p)]))
