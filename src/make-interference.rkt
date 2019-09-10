#!/usr/local/bin/racket
#lang racket

(require "graph.rkt")
(provide make-interference)
(provide interference-from-live)

(define (block? b)
  (match b
    [`(block ,info ,instrs ...) #t]
    [_ #f]))

(define (blocks? bs)
  (andmap block? bs))

(define (make-interference p)
  (match p
    [`(program ,locals ,(? blocks? blocks))
      (define interference-graph 
        (for/fold ([g '()])
                  ([b blocks])
          (define live-list (second b))
          (define instrs (drop b 2))
          (define useful-live-list (rest live-list))
          (cond
            ; check once to ensure that live-list length matches instrs length
            [(= (length useful-live-list) (length instrs))
             (interference-from-live useful-live-list instrs g)]
            [else (error "live-list and instrs not equal length")])))
        `(program ((locals ,locals) (conflicts ,interference-graph)) ,blocks)]
      ; chop off the first from live-list because who cares
    [_ (error "Bad input to make interference")]))

(define (get-var-from-arg arg)
  (match arg
    [`(var ,v) v]
    [`(reg ,r) arg]
    [else null]))

; Given a list of variables that are grouped by when they are alive e.g. ((), (x), (x y), (x y z))
; Construct an interference graph that looks like ((x (y z)) (y (x z)) (z (x y)))
; See pg 41 in Essentials of Compilation for details on building the interference graph
(define (interference-from-live live-list instrs graph)
  (cond
    ; base case - return the graph if no more instrs
    [(empty? instrs) graph]
    [else 
      (define new-graph
        (match (first instrs)
          ; If instr is an arithmetic operation, then add the edge (dst, var) for every var in the set L_after(k) unles var
          ; == dst
          ; TODO - I think we only care if dst is a var. if it's a reg, not sure. Same with arg1?
          [`(addq ,arg1 (var ,dst))
            (define new-edges (set-remove (first live-list) dst))
            (graph-add-multiple-edges graph dst new-edges)]
          [`(negq (var ,dst))
            (define new-edges (set-remove (first live-list) dst))
            (graph-add-multiple-edges graph dst new-edges)]

          ; If instr is a move, then add the edge (dst, var) for every var in L_after(k) unless v == dst or v == src
          [(or `(movq ,arg1 (var ,dst)) `(movzbq ,arg1 (var ,dst)))
            (define new-edges (set-remove (set-remove (first live-list) (get-var-from-arg arg1)) dst))
            (graph-add-multiple-edges graph dst new-edges)]

          [`(callq ,label)
            (define caller-save-regs '(rax rcx rdx rsi rdi r8 r9 r10 r11))
            (for/fold ([g graph])
                      ([var (first live-list)])
              (graph-add-multiple-saturation g var caller-save-regs))]

          ; Don't do anything for jump?
          [`(jmp ,label) graph]
          
          ; general case for when dst isn't a var - do nothing
          [`(,(? symbol? op) ,args ...) graph]

          [_ (error "Interference-from-live: Unrecognized instruction ~s" (first instrs))]))
      (interference-from-live (rest live-list) (rest instrs) new-graph)]))
