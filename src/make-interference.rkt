#!/usr/local/bin/racket
#lang racket

(require "graph.rkt")
(provide make-interference)
(provide interference-from-live)

(define (make-interference p)
  (match p
    [`(program ,info ((,label (block ,live-list ,instrs))))
      (cond
        ; check once to ensure that live-list length matches instrs length
        [(= (length live-list) (length instrs))
         `(program ,info ((,label (block ,(interference-from-live live-list instrs '()) ,instrs))))]
        [else (error "live-list and instrs not equal length")])]
    [_ (error "Bad input to make interference")]))

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
          ; TODO handle src/dst parsing better - probably want to check if it's a var or reg and not an int
          [`(addq (,_ ,src) (,_ ,dst))
            (define new-edges (remove dst (first live-list)))
            (graph-add-multiple-edges graph dst new-edges)]
          [`(negq (,_ ,dst))
            (define new-edges (remove dst (first live-list)))
            (graph-add-multiple-edges graph dst new-edges)]

          ; If instr is a move, then add the edge (dst, var) for every var in L_after(k) unless v == dst or v == src
          [`(movq (,_ ,src) (,_ ,dst)) 
            (define new-edges (remove dst (remove src (first live-list))))
            (graph-add-multiple-edges graph dst new-edges)]

          ; If instr is of the form (callq label), then add an edge (r, v) for every caller-saved register r and every
          ; variable v in L_after(k) [the interference graph also has registers themselves???? I thought registers were just
          ; the colors for this graph...]
          [`(callq ,label) (error "Unimplemented for callq")]

          ; Don't do anything for jump?
          [`(jmp ,label) (error "Unimplemented for jmp")]
          [_ (error "Interference-from-live: Unrecognized instruction ~s" (first instrs))]))
      (interference-from-live (rest live-list) (rest instrs) new-graph)]))
