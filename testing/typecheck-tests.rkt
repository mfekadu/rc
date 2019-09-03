#!/usr/bin/env racket
#lang racket
(require rackunit)
(require racket/contract)
(require "utilities.rkt") ; for check-fail and check-fail-with-name
(require "../src/typecheck.rkt")

; typecheck-exp tests
; simple exprs
(check-equal? (typecheck-exp '() 2) 'Integer)
(check-equal? (typecheck-exp '() #f) 'Boolean)
(check-equal? (typecheck-exp '() #t) 'Boolean)
(check-equal? (typecheck-exp '((x . Integer)) 'x) 'Integer)

; more complex exprs
(check-equal? (typecheck-exp '() '(+ 2 2)) 'Integer)
(check-equal? (typecheck-exp '() '(eq? 3 4)) 'Boolean)
(check-equal? (typecheck-exp '() '(eq? #f #t)) 'Boolean)

; stuff with let
(check-equal? (typecheck-exp '() '(let ([x 2]) x)) 'Integer)
(check-equal? (typecheck-exp '() '(let ([x (+ 4 1)]) (eq? x 5))) 'Boolean)

; err cases
(check-fail (lambda () (typecheck-exp '() 'x)))
(check-fail (lambda () (typecheck-exp '() '(eq? #f 4))))

(displayln "Typecheck tests finished")
