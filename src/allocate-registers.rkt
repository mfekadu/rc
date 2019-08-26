#!/usr/bin/env racket
#lang racket

(require "graph.rkt")
(require "../testing/utilities.rkt")
(provide allocate-registers)


; This is a maximum number of variables both in registers and on the stack
; Compilers should believe they have infinite memory, but let's like.. uh .. not do that for fun... lol
; this should probably be 8 or 16 or something???
(define MAX_VARIABLES 100)
(define REGISTERS '(rbx rcx rdx rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))

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
  (first (for/list ([i (in-range MAX_VARIABLES)]
        #:when (not (set-member? sat i)))
    i)))

(check-equal? (get-next-color (set)) 0)
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
  ; set up the colors mapping initially as -1 by mapping cons onto each local to make an association list
  (define colors (make-immutable-hash (map (λ (x) (cons x -1)) locals)))
  (define (helper g locals colors)

    (cond [(and (empty? locals) (andmap (λ (n) (>= n 0)) (hash-values colors)))
           colors]
          [(empty? locals) (error 'color-graph "something bad. empty locals? colors= ~v" colors)]
          [(andmap (λ (n) (>= n 0)) (hash-values colors)) (error 'color-graph "something bad. colors have -1? colors= ~v" colors)]
          [else

           ; get the node with the maximum saturation
           (define max-sat-node (get-max-sat g locals))
           ; and that node's vertex
           (define max-sat-vertex (first max-sat-node))
           ; and that node's saturation
           (define max-sat (second max-sat-node))
           ; and that node's neighbors
           (define neighbors (third max-sat-node))
           ; get the next possible color
           (define next-color (get-next-color max-sat))
           ; update the color of that node to the "next possible color"
           (define colors2 (hash-set colors max-sat-vertex next-color))
                      
           ; update the graph to have the "next-color" included in the saturation of all the "neighbors"
           (define updated_graph
             (for/fold ([gg g])
                       ([n neighbors])
               (graph-add-saturation gg n next-color)))
           
           (helper updated_graph (remove max-sat-vertex locals) colors2)]))

  (helper g locals colors))

(define (home-from-color c)
  (cond
    [(< c (length REGISTERS)) `(reg ,(list-ref REGISTERS c))]
    [else 
      (define offset (* -8 (+ 1 (- c (length REGISTERS)))))
      `(deref rbp ,offset)]))

; given a mapping of variables to their colors
; and a list of register names e.g. '((reg rcx) (reg rbx))
; and the code??
; return the code but with the registers replacing the appropriate var
; and spill extra variables on the stack if need be
(define (assign-registers var coloring)
  (home-from-color (hash-ref coloring var)))

; ari = allocate-registers-instructions
; given a list of x86 instruction
; return a list of x86 instruction with variables in registers or spilled on stack
(define (ari instrs coloring)
  (for/list ([i instrs])
    (for/list ([arg i])
      (match arg
        [`(var ,v) (assign-registers v coloring)]
        [_ arg]))))

; **************************************************
; allocate-registers
; **************************************************

; given an x86 program 
; return an x86 program with variables in registers or spilled on stack
(define (allocate-registers p)
  (match p
    [`(program ,locals (,label (block ,graph ,instrs)))
     `(program ,locals (,label (block () ,(ari instrs (color-graph graph locals)))))]
    [_ (error 'allocate-registers "Bad x86 program ~s " p)]))


; **************************************************
; TEST CASES
; **************************************************

(require rackunit)

; ==================================================
; TEST home from color 
; ==================================================

(check-equal? (home-from-color 0) '(reg rbx))
(check-equal? (home-from-color (length REGISTERS)) '(deref rbp -8))
(check-equal? (home-from-color (- (length REGISTERS) 1)) '(reg r15))


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

(define given1-cg `((z ,(set) ,(set 't.1 'y 'w))
                     (t.1 ,(set) ,(set 'z))
                     (w ,(set) ,(set 'z 'y 'x 'v))
                     (y ,(set) ,(set 'z 'x 'w))
                     (x ,(set) ,(set 'y 'w))
                     (v ,(set) ,(set 'w))))
(define given1-locals '(t.1 z w y x v))

(define given1-alloc-block `(block ,given1-cg ,given1-alloc-instrs))
(define given1-alloc-prog `(program ,given1-locals (start ,given1-alloc-block)))
(define expect1-alloc-block `(block () ,expect1-alloc-instrs))
(define expect1-alloc-prog `(program ,given1-locals (start ,expect1-alloc-block)))
(check-equal? (allocate-registers given1-alloc-prog) expect1-alloc-prog)

; ==================================================
; TEST ari
; ==================================================
; test a simple addq
#;(ari '((movq (int 2) (var x)
       (addq (int 2) (var x))
       (retq)))
     '())

; test ---
(check-true #t)


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

; ==================================================
; TEST assign-registers
; ==================================================
; test ---
(check-true #t)

; test ---
(check-true #t)
