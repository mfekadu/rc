#lang racket

(provide block?)
(provide blocks?)

(define (block? b)
  (match b
    [`(block ,info ,instrs ...) #t]
    [_ #f]))

(define (blocks? bs) (andmap block? bs))
