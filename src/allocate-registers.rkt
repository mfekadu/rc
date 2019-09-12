#!/usr/bin/env racket
#lang racket

(require "graph.rkt")
(provide allocate-registers)
(provide get-max-sat)
(provide get-next-color)
(provide color-graph)
(provide home-from-color)
(provide assign-registers)
(provide ari)
(provide REGISTERS)


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

; given an interference graph and a list of all variables in the program
; return a mapping of variables to their colors
; (as represented by numbers 0...N) where N is the number of variables
(define (color-graph g locals)
  ; set up the colors mapping initially as -1 by mapping cons onto each local to make an association list
  (define colors (make-immutable-hash (map (λ (x) (cons x -1)) locals)))
  (define (helper g locals colors)

    (cond 
      ; Base case
      ; If locals are empty and all the vertices have colors >=0, then we're done and return the colors
      [(and (empty? locals) (andmap (λ (n) (>= n 0)) (hash-values colors)))
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

  (cond
    ; if g is empty, then there are no conflicts so we should set all the colors to 0
    [(empty? g) (make-immutable-hash (map (λ (x) (cons x 0)) locals))]
    [else (helper g locals colors)]))

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
    [`(program ((locals ,locals) (conflicts ,graph)) ,blocks)
      (define colored-graph (color-graph graph locals))
      (define ret-blocks
        (for/list ([b blocks])
          (match b
            [`(block ,live ,instrs ...)
              (append `(block ()) (ari instrs colored-graph))]
            [_ (error 'allocate-registers "Malformed block! ~v" b)])))
     `(program () ,ret-blocks)]
    [_ (error 'allocate-registers "Bad x86 program ~s " p)]))
