#!/usr/bin/env racket
#lang racket
; **************************************************
; TEST CASES
; **************************************************

(require "../testing/utilities.rkt")
(require "../src/allocate-registers.rkt")
(require "../src/graph.rkt")
(require rackunit)

; ==================================================
; TEST home from color 
; ==================================================

(check-equal? (home-from-color 0) '(reg rbx))
(check-equal? (home-from-color (length REGISTERS)) '(deref rbp -8))
(check-equal? (home-from-color (- (length REGISTERS) 1)) '(reg r15))

(check-equal? (get-next-color (set)) 0)
(check-equal? (get-next-color (set 0)) 1)
(check-equal? (get-next-color (set 0 2)) 1)
(check-equal? (get-next-color (set 0 1)) 2)
(check-equal? (get-next-color (set 0 1 2 4)) 3)
(check-equal? (get-next-color (set 0 1 2 3 4)) 5)
(check-equal? (get-next-color (set 0 1 2 3 4 5 6 7)) 8)

; ==================================================
; TEST allocate-registers
; ==================================================
; test textbook program

(define given1-cg `((z ,(set) ,(set 't.1 'y 'w))
                     (t.1 ,(set) ,(set 'z))
                     (w ,(set) ,(set 'z 'y 'x 'v))
                     (y ,(set) ,(set 'z 'x 'w))
                     (x ,(set) ,(set 'y 'w))
                     (v ,(set) ,(set 'w))))
(define given1-locals '(t.1 z w y x v))

(define given1-alloc-block `(block () (label start)
                              (movq (int 1) (var v))     ; 2
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
                              (jmp conclusion)))

(define given1-alloc-prog `(program ((locals ,given1-locals) (conflicts ,given1-cg)) (,given1-alloc-block)))
(define expect1-alloc-block `(block () (label start)
                               (movq (int 1) (reg rcx))
                               (movq (int 46) (reg rbx))
                               (movq (reg rcx) (reg rcx))
                               (addq (int 7) (reg rcx))
                               (movq (reg rcx) (reg rdx))
                               (addq (int 4) (reg rdx))
                               (movq (reg rcx) (reg rcx))
                               (addq (reg rbx) (reg rcx))
                               (movq (reg rdx) (reg rbx))
                               (negq (reg rbx))
                               (movq (reg rcx) (reg rax))
                               (addq (reg rbx) (reg rax))
                               (jmp conclusion)))
(define expect1-alloc-prog `(program () (,expect1-alloc-block)))
(check-equal? (allocate-registers given1-alloc-prog) expect1-alloc-prog)

; i GUESS i have to put a branching case in here ugh
(define given5-blocks `((block 
                          ()
                          (label block176) (movq (int 0) (reg rax)) (jmp conclusion))
                        (block
                          ()
                          (label block178) (addq (var b) (var c)) (jmp block176))
                        (block
                          ()
                          (label block177) (movq (int 4) (var d)) (addq (var d) (var a)) (jmp block176))
                        (block
                          ()
                          (label start) (movq (int 1) (var a)) (movq (int 2) (var b)) (movq (int 3) (var c))
                          (cmpq (int 1) (int 1)) (jmp-if e block177) (jmp block178))))
                   
(define given5-graph `((b ,(set) ,(set 'a 'c))
                          (c ,(set) ,(set 'b 'a))
                          (a ,(set) ,(set 'b 'c 'd))
                          (d ,(set) ,(set 'a))))

(define given5 `(program ((locals (a b c d)) (conflicts ,given5-graph)) ,given5-blocks))
(check-equal? (allocate-registers given5)
              `(program () 
                        ((block 
                          ()
                          (label block176) (movq (int 0) (reg rax)) (jmp conclusion))
                        (block
                          ()
                          (label block178) (addq (reg rcx) (reg rdx)) (jmp block176))
                        (block
                          ()
                          (label block177) (movq (int 4) (reg rcx)) (addq (reg rcx) (reg rbx)) (jmp block176))
                        (block
                          ()
                          (label start) (movq (int 1) (reg rbx)) (movq (int 2) (reg rcx)) (movq (int 3) (reg rdx))
                          (cmpq (int 1) (int 1)) (jmp-if e block177) (jmp block178)))))


; ==================================================
; TEST ari
; ==================================================
; test a simple addq

; graph: `((t . .) (z . .))
;coloring: #hash((t . 0) (z . 0)))
(define (validate-coloring graph coloring)
  (cond
    [(empty? graph) #t]
    [else
     (define node (first graph))
     (define vertex (first node))
     (define neighbors (set->list (third node)))
     (define vertex-color (hash-ref coloring vertex))
     (if
      (andmap (λ (n) (not (equal? vertex-color (hash-ref coloring n)))) neighbors)
      (validate-coloring (rest graph) coloring)
      #f)]))

; given an actually valid coloring
(check-true (validate-coloring given1-cg #hash((t.1 . 0) (v . 1) (w . 0) (x . 1) (y . 2) (z . 1))))
; given an obviously bad coloring
(check-false (validate-coloring given1-cg #hash((t.1 . 1) (v . 1) (w . 1) (x . 1) (y . 1) (z . 1))))
; given a subtly bad coloring
(check-false (validate-coloring given1-cg #hash((t.1 . 0) (v . 0) (w . 0) (x . 1) (y . 2) (z . 1))))

; ==================================================
; TEST color-graph
; ==================================================
; TEST HELPERS ...
;
; list of all the caller save registers
;(define CALLER_SAVE_REGS '(rax rdx rcx rsi rdi r8 r9 r10 r11))
; list with just rax because rax will not get assigned to ??? necessary??
;(define RAX '(rax))

; test 1

(define vars '(t.1 z w y x v))
(check-true (validate-coloring given1-cg (color-graph given1-cg vars)))

; test get-max-sat
; test textbook example
(check-equal? (get-max-sat given1-cg vars) (graph-get-node given1-cg 't.1))
; test simple example
(check-equal? (get-max-sat `((v ,(set) ,(set))) '(v)) (graph-get-node `((v ,(set) ,(set))) 'v))
; test simple example with edge
(check-equal? (get-max-sat `((v ,(set) ,(set 'w)) 
                             (w ,(set) ,(set 'v))) '(v w))
              (graph-get-node `((v ,(set) ,(set 'w)) 
                                (w ,(set) ,(set 'v))) 'v))

(check-fail (λ () (color-graph `((z ,(set) ,(set))) '(z t))))

(displayln "allocate registers test done")
