#!/usr/local/bin/racket
#lang racket
(require rackunit)
(require racket/contract)
(require "test-helpers.rkt") ; for check-fail and check-fail-with-name
(require "../src/graph.rkt")

(define fc-graph '((x . (y z)) (y . (x z)) (z . (x y))))
(define empty-graph '())

(check-equal? (graph-contains fc-graph 'x) #t)
(check-equal? (graph-contains fc-graph 'a) #f)
(check-equal? (graph-contains empty-graph 'x) #f)

(displayln "Graph tests finished running")
