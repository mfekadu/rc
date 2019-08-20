#!/usr/local/bin/racket
#lang racket
(require rackunit)
(require racket/contract)
(require "utilities.rkt") ; for check-fail and check-fail-with-name
(require "../src/graph.rkt")

(define fc-graph `((x . ,(set 'y 'z)) (y . ,(set 'x 'z)) (z . ,(set 'x 'y))))
(define empty-graph '())

; graph-contains? tests
(check-equal? (graph-contains? fc-graph 'x) #t)
(check-equal? (graph-contains? fc-graph 'z) #t)
(check-equal? (graph-contains? fc-graph 'a) #f)
(check-equal? (graph-contains? empty-graph 'x) #f)

; graph-get-edges tests
(check-equal? (graph-get-edges fc-graph 'x) (set 'y 'z))
(check-equal? (graph-get-edges fc-graph 'z) (set 'x 'y))
(check-fail (lambda () (graph-get-edges fc-graph 'a)))
(check-fail (lambda () (graph-get-edges empty-graph 'z)))

; graph-insert tests
(check-equal? (graph-insert empty-graph 'x '(y z)) `((x . ,(set 'y 'z))))

; graph-remove test
(check-equal? (graph-remove fc-graph 'z) `((x . ,(set 'y 'z)) (y . ,(set 'x 'z))))
(check-equal? (graph-remove fc-graph 'a) fc-graph)
(check-equal? (graph-remove empty-graph 'x) empty-graph)

; graph-add-edge-one-way tests
; add existing edge
(check-equal? (graph-add-edge-one-way fc-graph 'x 'y) fc-graph)
; add edge to completely new node
(check-equal? (graph-add-edge-one-way fc-graph 'a 'z) `((a . ,(set 'z)) (x . ,(set 'y 'z)) (y . ,(set 'x 'z)) (z . ,(set 'x 'y))))
; add new edge to existing node
(check-equal? (graph-add-edge-one-way fc-graph 'x 'a) `((x . ,(set 'a 'y 'z)) (y . ,(set 'x 'z)) (z . ,(set 'x 'y))))
; add new node to empty list
(check-equal? (graph-add-edge-one-way empty-graph 'x 'y) `((x . ,(set 'y))))

; case where g isn't a list
(check-fail (lambda () (graph-add-edge-one-way 2 'x 'y)))

; graph-add-edge tests
(check-equal? (graph-add-edge empty-graph 'x 'y) `((y . ,(set 'x)) (x . ,(set 'y))))
(check-equal? (graph-add-edge fc-graph 'x 'a)`((a . ,(set 'x)) (x . ,(set 'a 'y 'z)) (y . ,(set 'x 'z)) (z . ,(set 'x 'y))))

; unfortunately it no longer preserves order like it did before but that's not important
(check-equal? (graph-add-edge fc-graph 'x 'y) `((y . ,(set 'x 'z)) (x . ,(set 'y 'z)) (z . ,(set 'x 'y))))

; for the case where both x and y exist but have no edges between each other
(define not-fc-graph `((x . ,(set)) (y . ,(set))))
(check-equal? (graph-add-edge not-fc-graph 'x 'y) `((y . ,(set 'x)) (x . ,(set 'y))))
; case where g isn't a list
(check-fail (lambda () (graph-add-edge 'x 'y 'z)))

; testing adding multiple edges
(check-equal? (graph-add-multiple-edges empty-graph 'x '(a b c)) `((c . ,(set 'x)) (x . ,(set 'c 'b 'a)) (b . ,(set 'x))
                                                                                   (a . ,(set 'x))))

(check-equal? (graph-add-multiple-edges not-fc-graph 'y '(a x z)) `((z . ,(set 'y)) (y . ,(set 'z 'x 'a)) (x . ,(set 'y))
                                                                                    (a . ,(set 'y))))

(displayln "Graph tests finished running")
