#!/usr/local/bin/racket
#lang racket

(provide graph-contains?)
(provide graph-get-edges)
(provide graph-add-edge)
(provide graph-add-edge-one-way)
(provide graph-insert)
(provide graph-remove)
(provide graph-add-multiple-edges)

; simple graph library for make-interference pass

; graphs are alists that look like ((x . (set a b)) (a . (set x)) (b . (set x))), where the first element is a vertex and the second
; element is a set of edges which the vertex is connected to.
; graphs are undirected

; return true if v in g else false
(define (graph-contains? g v)
  (cond
    [(not (list? g)) (error "what are you doing g is not a list")]
    [(empty? g) #f]
    [(equal? (car (first g)) v) #t]
    [else (graph-contains? (rest g) v)]))

(define (graph-get-edges g v)
  (cond 
    ; TODO - should this error if we get an empty list?
    [(empty? g) (error "ERROR tried to get edges from vertex not in graph")]
    [(equal? (car (first g)) v) (cdr (first g))]
    [else (graph-get-edges (rest g) v)]))

; Creates new element for v in g with edges e
; e can be either a list or a set
(define (graph-insert g v e)
  (cond
    [(list? e) (cons (cons v (list->set e)) g)]
    [(set? e) (cons (cons v e) g)]
    [else (error "graph-insert error: expected list or set but got ~s" e)]))

; removes first element for vertex v in graph g
; does not remove any other references to v that may still exist in g
; TODO i totally wasn't lazy with these variable names 
(define (graph-remove g v)
  (remove v g (lambda (a b) (equal? a (car b)))))

; adds edge from v1 to v2, but not the other way around
(define (graph-add-edge-one-way g v1 v2)
  (cond
    [(not (list? g)) (error "g is not a list")]
    ; if v1 already in g, just append v2 to the existing set if necessary
    [(graph-contains? g v1)
      (define old-edges (graph-get-edges g v1))
      (define new-graph (graph-remove g v1))
      (graph-insert new-graph v1 (set-add old-edges v2))]
    ; v1 not in g, create an entirely new element
    [else (graph-insert g v1 (set v2))]))

; adds edge from v1 to v2 and from v2 to v1
(define (graph-add-edge g v1 v2)
  (define after-v1-added (graph-add-edge-one-way g v1 v2))
  (graph-add-edge-one-way after-v1-added v2 v1))

; adds a bunch of edges to v in the graph g
(define (graph-add-multiple-edges g v edges)
  (for/fold ([graph g])
            ([e edges])
    (graph-add-edge graph v e)))
