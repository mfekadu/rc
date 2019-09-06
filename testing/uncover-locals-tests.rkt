#!/usr/bin/env racket
#lang racket
(require rackunit)
(require "../src/uncover-locals.rkt")

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
(define given1-prog `(program () ((start ,given1))))
(check-equal? (uncover-locals given1-prog) `(program (x1 x2 y) ((start ,given1))))

(define given2-prog `(program () ((start ,given2))))
(check-equal? (uncover-locals given2-prog) given2-prog)

(define given-assign-if '((start (if (eq? #t #t) (goto L1) (goto L2)))
                 (L1 (seq (assign x 2) (goto L3)))
                 (L2 (seq (assign x 3) (goto L3)))
                 (L3 (return x))))

(define given-assign-if-prog `(program () ,given-assign-if))
(check-equal? (uncover-locals given-assign-if-prog) `(program (x) ,given-assign-if))

(check-exn exn:fail? (lambda () (uncover-locals given3)))

(displayln "uncover locals tests finished")
