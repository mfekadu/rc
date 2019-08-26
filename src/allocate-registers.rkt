#!/usr/bin/env racket
#lang racket

(require "graph.rkt")
(provide allocate-registers)


; **************************************************
; HELPERS
; **************************************************

; TODO: add this to graph.rkt
; given a graph and the list of variables
; return the most saturated node
(define (get-max-sat g vars)
  (graph-get-node g (cdr (argmax car
                               (map (λ (v)
                                      (define s (graph-get-saturation g v))
                                      (cons (length (set->list s)) v))
                                    vars)))))

; given a saturation set
; return the number that represents the next available color
(define (get-next-color sat)
  (first (for/list ([i (in-range 100)]
        #:when (not (set-member? sat i)))
    i)))

(check-equal? (get-next-color (set 0)) 1)
(check-equal? (get-next-color (set 0 2)) 1)
(check-equal? (get-next-color (set 0 1)) 2)
(check-equal? (get-next-color (set 0 1 2 4)) 3)
(check-equal? (get-next-color (set 0 1 2 3 4)) 5)
(check-equal? (get-next-color (set 0 1 2 3 4 5 6 7)) 8)

; given an interference graph and a list of all variables in the program
; return a mapping of variables to their colors
; (as represented by numbers 0...N) where N is the number of variables
(define (color-graph g locals)
  (define colors (make-immutable-hash (map (λ (x) (cons x -1)) locals)))
  (define max-sat-node (get-max-sat g locals))
  (define max-sat-vertex (car max-sat-node))
  (define max-sat (second max-sat-node))
  (define neighbors (graph-get-edges g (first max-sat-node)))
  (define next-color (get-next-color max-sat))
  (define colors2 (hash-set colors max-sat-vertex next-color))

  ; TODO: find a more optimal solution
  ;       because remove and insert would be slow
  ;          consider graph as mutable list
  ;       OR consider graph as immutable hash?
  ;       OR consider any other faster solution to update edges
  (define updated_graph (foldl (λ (node grph)
           (cond
             [(set-member? neighbors (first node))
              (define updated_node
                `(,(first node) ,(set-add (second node) next-color) ,(third node)))
              (cons updated_node grph)]
             [else (cons node grph)]))
         '()
         g))

  (displayln "updated graph...")
  (displayln updated_graph)
  (display "colors... ")
  (displayln colors)
  (displayln colors2)
  (display "graph... ")
  (displayln g)
  (display "max-sat-node... ")
  (displayln max-sat-node)
  (display "neighbors... ")
  (displayln neighbors))

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
; TEST ari
; ==================================================
; test a simple addq
(ari '((movq (int 2) (var x)
       (addq (int 2) (var x))
       (retq))))

; test ---
(check-true #t)

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
(define given1-cg `((z ,(set) ,(set 't.1 'y 'w))
                     (t.1 ,(set) ,(set 'z))
                     (w ,(set) ,(set 'z 'y 'x 'v))
                     (y ,(set) ,(set 'z 'x 'w))
                     (x ,(set) ,(set 'y 'w))
                     (v ,(set) ,(set 'w))))
(define expect1-cg
  (make-hash '((v . 0) (w . 2) (x . 1) (y . 0) (z . 1) (t.1 . 0))))

;(hash? (make-hash expect1-cg))
;(hash? (make-hash (map (λ (x) (cons x -1)) '(x y z))))
(define vars '(t.1 z w y x v))
(check-equal? (color-graph given1-cg vars) expect1-cg)

; test get-max-sat
; test textbook example
(check-equal? (get-max-sat given1-cg vars) (graph-get-node given1-cg 'z))
; test simple example
(check-equal? (get-max-sat `((v ,(set) ,(set))) '(v)) (graph-get-node `((v ,(set) ,(set))) 'v))
; test simple example with edge
(check-equal? (get-max-sat `((v ,(set) ,(set 'w)) 
                             (w ,(set) ,(set 'v))) '(v w))
              (graph-get-node `((v ,(set) ,(set 'w)) 
                                (w ,(set) ,(set 'v))) 'v))

; ==================================================
; TEST assign-registers
; ==================================================
; test ---
(check-true #t)

; test ---
(check-true #t)