#!/usr/bin/env racket
#lang racket
(require rackunit)
(provide uncover-locals)
(provide get-locals-from-tail)

(define (uncover-locals c0-prog)
  (match c0-prog
    [`(program ,(? empty?) ,tails) 
      `(program ,(get-locals-from-tails tails) ,tails)]
    [_ (error "oh no")])) 

(define (get-locals-from-tails c0-tails)
  (set->list
    (list->set
      (flatten
        (map (lambda (x) (get-locals-from-tail (second x))) c0-tails)))))

; takes a c0 expr and returns a list of locals used in the program
(define (get-locals-from-tail c0-expr)
  (match c0-expr
    [`(return ,expr) '()]
    [`(seq (assign ,var ,val) ,tail) (cons var (get-locals-from-tail tail))]
    [`(goto ,label) '()]
    [`(if ,cnd ,thn ,els) `(,(get-locals-from-tail thn) ,(get-locals-from-tail els))]
    [_ (error "whoops!")]))

; (let ([x (if #t 1 2)]) x) 
