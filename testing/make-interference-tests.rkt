#!/usr/local/bin/racket
#lang racket
(require rackunit)
(require racket/contract)
(require "test-helpers.rkt") ; for check-fail and check-fail-with-name
(require "../src/make-interference.rkt")

(define instrs1 '((movq (int 1) (var v))
                  (movq (int 46) (var w))
                  (movq (var v) (var x))
                  (addq (int 7) (var x))))
(define live-list1 '((v) (v w) (w x) (w x)))
(define expect1 '((w . (x v))     ; w interferes with v on instr 2
                  (x . (w))       ; x interferes with w on instrs 3 and 4
                  (v . (w))))

(check-equal? (interference-from-live live-list1 instrs1 '()) expect1)

; full example from pg 40 except the register parts that idk how to handle
(define instrs2 '((movq (int 1) (var v))
                  (movq (int 46) (var w))
                  (movq (var v) (var x))
                  (addq (int 7) (var x))
                  (movq (var x) (var y))
                  (addq (int 4) (var y))
                  (movq (var x) (var z))
                  (addq (var w) (var z))
                  (movq (var y) (var t.1))
                  (negq (var t.1))))
(define live-list2 '((v)
                     (v w)
                     (w x)
                     (w x)
                     (w x y)
                     (w x y)
                     (w y z)
                     (y z)
                     (z t.1)
                     (z t.1)))
(define expect2 '((z . (t.1 y w))
                  (t.1 . (z))
                  (y . (z x w))
                  (w . (z y x v))
                  (x . (y w))
                  (v . (w))))
(check-equal? (interference-from-live live-list2 instrs2 '()) expect2)
(displayln "make interference tests done")
