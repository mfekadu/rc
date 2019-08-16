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
