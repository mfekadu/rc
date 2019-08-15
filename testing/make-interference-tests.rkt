#!/usr/local/bin/racket
#lang racket
(require rackunit)
(require racket/contract)
(require "test-helpers.rkt") ; for check-fail and check-fail-with-name
(require "../src/make-interference.rkt")

(define given1)
