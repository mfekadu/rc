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

(displayln "Graph tests finished running")
