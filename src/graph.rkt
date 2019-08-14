#!/usr/local/bin/racket
#lang racket

(provide graph-contains?)
(provide graph-get-edges)
(provide graph-add-edge)

; simple graph library for make-interference pass

; graphs are alists that look like ((x (a b)) (a (x)) (b (x))), where the first element is a vertex and the second
; element is a list of edges which the vertex is connected to.
; graphs are undirected

; return true if v in g else false
(define (graph-contains? g v)
  (cond
    [(empty? g) #f]
    [(equal? (car (first g)) v) #t]
    [else (graph-contains? (rest g) v)]))

(define (graph-get-edges g v)
  (cond 
    ; TODO - should this error if we get an empty list?
    [(empty? g) (error "ERROR tried to get edges from vertex not in graph")]
    [(equal? (car (first g)) v) (cdr (first g))]
    [else (graph-get-edges (rest g) v)]))

(define (graph-add-edge g v1 v2)
  ; inner function specifically adds edge from v1 to v2
  (define (graph-add-edge-one-way g v1 v2)
    (cond
      [(graph-contains? g v1) (displayln "unimplmeneted")]
      [else (displayln "unimplmented")]))

  (define after-v1-added (graph-add-edge-one-way g v1 v2))
  (graph-add-edge-one-way g v2 v1))
