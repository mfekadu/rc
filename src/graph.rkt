#!/usr/local/bin/racket
#lang racket

(provide graph-contains?)
(provide graph-get-edges)
(provide graph-add-edge)
(provide graph-add-edge-one-way)
(provide graph-insert)
(provide graph-remove)
(provide graph-add-multiple-edges)
(provide graph-get-saturation)
(provide graph-get-node)
(provide graph-add-saturation)
(provide graph-add-multiple-saturation)

; simple (undirected) graph library for make-interference pass

; graphs are alists that look like
;           ((x () (set a b)) (a (rax rbx) (set x)) (b (rax rcx rdi) (set x)))
; the first element is the name of the vertex
; the second element is the saturation of the vertex
; the third element is a set of edges which the vertex is connected to

; return true if v in g else false
(define (graph-contains? g v)
  (cond
    [(not (list? g)) (error "what are you doing g is not a list")]
    [(empty? g) #f]
    [(equal? (car (first g)) v) #t]
    [else (graph-contains? (rest g) v)]))

(define (graph-get-edges g v)
  (cond 
    [(empty? g) (error 'graph-get-edges "ERROR tried to get edges from vertex not in graph")]
    ; each vertex is structured like
    ; (x (saturation) (edges))
    ; so we want the third thing
    [(equal? (first (first g)) v) (third (first g))]
    [else (graph-get-edges (rest g) v)]))

(define (graph-get-saturation g v)
  (cond 
    [(empty? g) (error 'graph-get-saturation "ERROR tried to get edges from vertex { ~v } not in graph" v)]
    ; each vertex is structured like
    ; (x (saturation) (edges))
    ; so we want the second thing
    [(equal? (first (first g)) v) (second (first g))]
    [else (graph-get-saturation (rest g) v)]))


(define (graph-get-node g v)
  (cond 
    [(empty? g) (error 'graph-get-node "ERROR tried to get edges from vertex not in graph")]
    ; each vertex is structured like
    ; (x (saturation) (edges))
    ; so we want the second thing
    [(equal? (first (first g)) v) (first g)]
    [else (graph-get-node (rest g) v)]))

; Creates new element for v in g with edges e
; e can be either a list or a set
(define (graph-insert g v s e)
  (define edges
    (cond
      [(list? e) (list->set e)]
      [(set? e) e]
      [else (error "graph-insert error: edges must be a list or set but got ~s" e)]))
  (define saturation
    (cond
      [(list? s) (list->set s)]
      [(set? s) s]
      [else (error "graph-insert error: saturation must be a list or set but got ~s" e)]))
  (cons (list v saturation edges) g))

; removes first element for vertex v in graph g
; does not remove any other references to v that may still exist in g
; TODO i totally wasn't lazy with these variable names 
(define (graph-remove g v)
  (remove v g (lambda (a b) (equal? a (first b)))))

; adds edge from v1 to v2, but not the other way around
(define (graph-add-edge-one-way g v1 v2)
  (cond
    [(not (list? g)) (error "g is not a list")]
    ; if v1 already in g, just append v2 to the existing set if necessary
    [(graph-contains? g v1)
      (define old-edges (graph-get-edges g v1))
      (define old-sat (graph-get-saturation g v1))
      (define new-graph (graph-remove g v1))
      (graph-insert new-graph v1 old-sat (set-add old-edges v2))]
    ; v1 not in g, create an entirely new element
    [else (graph-insert g v1 (set) (set v2))]))

; adds edge from v1 to v2 and from v2 to v1
(define (graph-add-edge g v1 v2)
  (define after-v1-added (graph-add-edge-one-way g v1 v2))
  (graph-add-edge-one-way after-v1-added v2 v1))

; adds a bunch of edges to v in the graph g
(define (graph-add-multiple-edges g v edges)
  ; adding node for every vertex even if there are no conflicts
  ; because the future pass needs this
  ;(define new-g (graph-insert g v '() '()))
  (define new-g 
    (cond
      [(graph-contains? g v) g]
      [else (graph-insert g v '() '())]))
  (for/fold ([graph new-g])
            ([e edges])
    (graph-add-edge graph v e)))

(define (graph-add-saturation g v1 color)
  (cond
    [(not (list? g)) (error "g is not a list")]
    [(graph-contains? g v1)
      (define old-edges (graph-get-edges g v1))
      (define old-sat (graph-get-saturation g v1))
      (define new-graph (graph-remove g v1))
      (graph-insert new-graph v1 (set-add old-sat color) old-edges)]
    [else (graph-insert g v1 (set color) (set))]))

(define (graph-add-multiple-saturation g v saturation)
  (for/fold ([graph g])
            ([s saturation])
    (graph-add-saturation graph v s)))

