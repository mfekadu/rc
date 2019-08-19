#!/usr/bin/env racket
#lang racket
(require rackunit)
(provide uncover-locals)
(provide get-locals-from-tail)

(define (uncover-locals c0-prog)
  (match c0-prog
    [`(program ,(? empty?) (,label ,tail)) 
      `(program ,(get-locals-from-tail tail) (,label ,tail))]
    [_ (error "oh no")])) 

; takes a c0 expr and returns a list of locals used in the program
(define (get-locals-from-tail c0-expr)
  (match c0-expr
    [`(return ,expr) '()]
    [`(seq (assign ,var ,val) ,tail) (cons var (get-locals-from-tail tail))]
    [_ (error "whoops!")]))

