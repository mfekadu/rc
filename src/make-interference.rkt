#!/usr/local/bin/racket
#lang racket

(require "graph.rkt")
(provide make-interference)
(provide interference-from-live)

(define (make-interference p)
  (match p
    [`(program ,info ((,label (block ,live-list ,instrs))))
      `(program ,info ((,label (block ,(interference-from-live live-list instrs) ,instrs))))]
    [_ (error "Bad input to make interference")]))

; Given a list of variables that are grouped by when they are alive e.g. ((), (x), (x y), (x y z))
; Construct an interference graph that looks like ((x (y z)) (y (x z)) (z (x y)))
; See pg 41 in Essentials of Compilation for details on building the interference graph
(define (interference-from-live live-list instrs)
  ; iterate through both live-list and instrs
  (for/fold ([interference-graph '()])
            ([live-vars live-list]
             [instr instrs])
    (match instr
      ; If instr is an arithmetic operation, then add the edge (dst, var) for every var in the set L_after(k) unles var
      ; == dst
      ; TODO handle src/dst parsing better - probably want to check if it's a var or reg and not an int
      [`(addq (,_ ,src) (,_ ,dst))
        (define new-edges (remove dst live-list))
        (graph-add-multiple-edges interference-graph dst new-edges)]

      ; If instr is a move, then add the edge (dst, var) for every v in L_after(k) unless v == dst or v == src
      [`(movq (,_ ,src) (,_ ,dst)) 
        (define new-edges (remove dst (remove src live-list)))
        (graph-add-multiple-edges interference-graph dst new-edges)]

      ; If instr is of the form (callq label), then add an edge (r, v) for every caller-saved register r and every
      ; variable v in L_after(k) [the interference graph also has registers themselves???? I thought registers were just
      ; the colors for this graph...]
      [`(callq ,label) (error "Unimplemented for callq")]

      ; Don't do anything for jump?
      [`(jmp ,label) (error "Unimplemented for jmp")]
      [_ (error "Interference-from-live: Unrecognized instruction ~s" instr)])))

