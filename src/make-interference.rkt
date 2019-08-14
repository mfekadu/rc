#!/usr/local/bin/racket
#lang racket

(require "graph.rkt")

(define (make-interference p)
  (match p
    [`(program ,info ((,label (block ,live-list ,instrs))))
      `(program ,info ((,label (block ,(interference-from-live live-list instrs) ,instrs))))]
    [_ (error "Bad input to make interference")]))

; Given a list of variables that are grouped by when they are alive e.g. ((), (x), (x y), (x y z))
; Construct an interference graph that looks like ((x (y z)) (y (x z)) (z (x y)))
(define (interference-from-live live-list instrs)
  (displayln "unimplemented"))

