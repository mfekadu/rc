#!/usr/bin/racket
#lang racket
(require rackunit)

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

; TEST CASES
(define given1 '(seq (assign x1 20)
                (seq (assign x2 22)
                (seq (assign y (+ x1 x2))
                (return y)))))

; get-locals-from-tail tests
(check-equal? (get-locals-from-tail given1) '(x1 x2 y))

(define given2 '(return 12))
(check-equal? (get-locals-from-tail given2) '())

(define given3 'asldkfjaslkdjflkjasdf)
(check-exn exn:fail? (lambda () (get-locals-from-tail given3)))


; uncover locals tests
(define given1-prog `(program () (start ,given1)))
(check-equal? (uncover-locals given1-prog) `(program (x1 x2 y) (start ,given1)))

(define given2-prog `(program () (start ,given2)))
(check-equal? (uncover-locals given2-prog) given2-prog)

(check-exn exn:fail? (lambda () (uncover-locals given3)))
