#!/usr/local/bin/racket
#lang racket
(require rackunit)
(require racket/contract)
(require "test-helpers.rkt") ; for check-fail and check-fail-with-name
(require "../src/graph.rkt")

(define fc-graph '((x . (y z)) (y . (x z)) (z . (x y))))
(define empty-graph '())

; graph-contains? tests
(check-equal? (graph-contains? fc-graph 'x) #t)
(check-equal? (graph-contains? fc-graph 'z) #t)
(check-equal? (graph-contains? fc-graph 'a) #f)
(check-equal? (graph-contains? empty-graph 'x) #f)

; graph-get-edges tests
(check-equal? (graph-get-edges fc-graph 'x) '(y z))
(check-equal? (graph-get-edges fc-graph 'z) '(x y))
(check-fail (lambda () (graph-get-edges fc-graph 'a)))
(check-fail (lambda () (graph-get-edges empty-graph 'z)))

; graph-insert tests
(check-equal? (graph-insert empty-graph 'x '(y z)) '((x . (y z))))

; graph-remove test
(check-equal? (graph-remove fc-graph 'z) '((x . (y z)) (y . (x z))))
(check-equal? (graph-remove fc-graph 'a) fc-graph)
(check-equal? (graph-remove empty-graph 'x) empty-graph)

; graph-add-edge-one-way tests

; add existing edge
(check-equal? (graph-add-edge-one-way fc-graph 'x 'y) fc-graph)

; add edge to completely new node
(check-equal? (graph-add-edge-one-way fc-graph 'a 'z) '((a . (z)) (x . (y z)) (y . (x z)) (z . (x y))))

; add new edge to existing node
(check-equal? (graph-add-edge-one-way fc-graph 'x 'a) '((x . (a y z)) (y . (x z)) (z . (x y))))

; add new node to empty list
(check-equal? (graph-add-edge-one-way empty-graph 'x 'y) '((x . (y))))

(displayln "Graph tests finished running")
