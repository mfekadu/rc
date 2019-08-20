#!/usr/local/bin/racket
#lang racket

(require "graph.rkt")
(provide make-interference)
(provide interference-from-live)

(define (make-interference p)
  (match p
    [`(program ,info ((,label (block ,live-list ,instrs))))
      (define useful-live-list (rest live-list))
      (cond
        ; chop off the first from live-list because who cares

        ; check once to ensure that live-list length matches instrs length
        [(= (length useful-live-list) (length instrs))
         `(program ,info ((,label (block ,(interference-from-live useful-live-list instrs '()) ,instrs))))]
        [else (error "live-list and instrs not equal length")])]
    [_ (error "Bad input to make interference")]))

;
(define (get-var-from-arg arg)
  (match arg
    [`(var ,v) v]
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
          [`(movq ,arg1 (var ,dst)) 
            (define new-edges (set-remove (set-remove (first live-list) (get-var-from-arg arg1)) dst))
            (graph-add-multiple-edges graph dst new-edges)]

          ; If instr is of the form (callq label), then add an edge (r, v) for every caller-saved register r and every
          ; variable v in L_after(k) [the interference graph also has registers themselves???? I thought registers were just
          ; the colors for this graph...]
          [`(callq ,label) (error "Unimplemented for callq")]

          ; Don't do anything for jump?
          [`(jmp ,label) graph]
          
          ; general case for when dst isn't a var - do nothing
          [`(,(? symbol? op) ,args ...) graph]

          [_ (error "Interference-from-live: Unrecognized instruction ~s" (first instrs))]))
      (interference-from-live (rest live-list) (rest instrs) new-graph)]))
