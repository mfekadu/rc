#!/usr/local/bin/racket
#lang racket
(require rackunit)
(require racket/contract)
(require "utilities.rkt") ; for check-fail and check-fail-with-name
(require "../src/graph.rkt")

(define fc-graph `((x ,(set 'rbx 'rcx) ,(set 'y 'z)) (y ,(set 'rdx 'rdi 'rsi) ,(set 'x 'z)) (z ,(set) ,(set 'x 'y))))
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

; graph-get-saturation tests
(check-equal? (graph-get-saturation fc-graph 'x) (set 'rbx 'rcx))
(check-equal? (graph-get-saturation fc-graph 'z) (set))
(check-fail (lambda () (graph-get-saturation fc-graph 'a)))
(check-fail (lambda () (graph-get-saturation empty-graph 'z)))

; graph-get-node tests
(check-equal? (graph-get-node fc-graph 'x) `(x ,(set 'rcx 'rbx) ,(set 'y 'z)))
(check-equal? (graph-get-node fc-graph 'z) `(z ,(set) ,(set 'x 'y)))
(check-fail (lambda () (graph-get-node fc-graph 'a)))
(check-fail (lambda () (graph-get-node empty-graph 'z)))

; graph-insert tests
(check-equal? (graph-insert empty-graph 'x '(rcx) '(y z)) `((x ,(set 'rcx) ,(set 'y 'z))))
(check-equal? (graph-insert empty-graph 'x '() '(y z)) `((x ,(set) ,(set 'y 'z))))
(check-equal? (graph-insert empty-graph 'x '(rcx) '()) `((x ,(set 'rcx) ,(set))))

; graph-remove test
(check-equal? (graph-remove fc-graph 'z) `((x ,(set 'rbx 'rcx) ,(set 'y 'z))
                                           (y ,(set 'rdx 'rdi 'rsi) ,(set 'x 'z))))
(check-equal? (graph-remove fc-graph 'a) fc-graph)
(check-equal? (graph-remove empty-graph 'x) empty-graph)

; graph-add-edge-one-way tests
; add existing edge
(check-equal? (graph-add-edge-one-way fc-graph 'x 'y) fc-graph)
; add edge to completely new node
(check-equal? (graph-add-edge-one-way fc-graph 'a 'z) `((a ,(set) ,(set 'z))
                                                        (x ,(set 'rbx 'rcx) ,(set 'y 'z))
                                                        (y ,(set 'rdx 'rdi 'rsi) ,(set 'x 'z))
                                                        (z ,(set) ,(set 'x 'y))))
; add new edge to existing node
(check-equal? (graph-add-edge-one-way fc-graph 'x 'a) `((x ,(set 'rbx 'rcx) ,(set 'a 'y 'z))
                                                        (y ,(set 'rdx 'rdi 'rsi) ,(set 'x 'z))
                                                        (z ,(set) ,(set 'x 'y))))
; add new node to empty list
(check-equal? (graph-add-edge-one-way empty-graph 'x 'y) `((x ,(set) ,(set 'y))))

; case where g isn't a list
(check-fail (lambda () (graph-add-edge-one-way 2 'x 'y)))

; graph-add-edge tests
(check-equal? (graph-add-edge empty-graph 'x 'y) `((y ,(set) ,(set 'x))
                                                   (x ,(set) ,(set 'y))))
(check-equal? (graph-add-edge fc-graph 'x 'a)`((a ,(set) ,(set 'x))
                                               (x ,(set 'rbx 'rcx) ,(set 'a 'y 'z))
                                               (y ,(set 'rdx 'rdi 'rsi) ,(set 'x 'z))
                                               (z ,(set) ,(set 'x 'y))))

; unfortunately it no longer preserves order like it did before but that's not important
(check-equal? (graph-add-edge fc-graph 'x 'y) `((y ,(set 'rdx 'rdi 'rsi) ,(set 'x 'z))
                                                (x ,(set 'rbx 'rcx) ,(set 'y 'z))
                                                (z ,(set) ,(set 'x 'y))))

; for the case where both x and y exist but have no edges between each other
(define not-fc-graph `((x ,(set) ,(set)) (y ,(set) ,(set))))
(check-equal? (graph-add-edge not-fc-graph 'x 'y) `((y ,(set) ,(set 'x))
                                                    (x ,(set) ,(set 'y))))
; case where g isn't a list
(check-fail (lambda () (graph-add-edge 'x 'y 'z)))

; testing adding multiple edges
(check-equal? (graph-add-multiple-edges empty-graph 'x '(a b c)) `((c ,(set) ,(set 'x))
                                                                   (x ,(set) ,(set 'c 'b 'a))
                                                                   (b ,(set) ,(set 'x))
                                                                   (a ,(set) ,(set 'x))))

(check-equal? (graph-add-multiple-edges not-fc-graph 'y '(a x z)) `((z ,(set) ,(set 'y))
                                                                    (y ,(set) ,(set 'z 'x 'a))
                                                                    (x ,(set) ,(set 'y))
                                                                    (a ,(set) ,(set 'y))))

; testing graph-add-saturation
(check-equal? (graph-add-saturation not-fc-graph 'x 'rax) `((x ,(set 'rax) ,(set))
                                                            (y ,(set) ,(set))))
(check-equal? (graph-add-saturation not-fc-graph 'z 'rcx) `((z ,(set 'rcx) ,(set))
                                                            (x ,(set) ,(set))
                                                            (y ,(set) ,(set))))
(check-equal? (graph-add-saturation fc-graph 'x 'rdi) `((x ,(set 'rbx 'rcx 'rdi) ,(set 'y 'z))
                                                        (y ,(set 'rdx 'rdi 'rsi) ,(set 'x 'z))
                                                        (z ,(set) ,(set 'x 'y))))
(check-equal? (graph-add-saturation fc-graph 'y 'rdi) `((y ,(set 'rdx 'rdi 'rsi) ,(set 'x 'z))
                                                        (x ,(set 'rbx 'rcx) ,(set 'y 'z))
                                                        (z ,(set) ,(set 'x 'y))))


; testing graph-add-multiple-saturation
(check-equal? (graph-add-multiple-saturation not-fc-graph 'x '(rax rbx rcx)) `((x ,(set 'rax 'rbx 'rcx) ,(set))
                                                                             (y ,(set) ,(set))))

(displayln "Graph tests finished running")
