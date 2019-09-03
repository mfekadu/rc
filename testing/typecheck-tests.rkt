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
(check-equal? (typecheck-exp '() '(eq? #f 0)) 'Boolean)

; stuff with let
(check-equal? (typecheck-exp '() '(let ([x 2]) x)) 'Integer)
(check-equal? (typecheck-exp '() '(let ([x (+ 4 1)]) (eq? x 5))) 'Boolean)

; err cases
(check-fail (lambda () (typecheck-exp '() 'x)))
(check-fail (lambda () (typecheck-exp '() '(+ #f #t))))
(check-fail (lambda () (typecheck-exp '() '(let ([x #f]) (+ x 2)))))

; what's going to happen with variable shadowing?
; this is supposed to be called before uniquify
; Apparently this works, but won't there be 2 entries for x in the alist. What does dict-ref do when that happens?
(check-fail (lambda () (typecheck-exp '() '(let ([x (+ 2 2)])
                                             (let ([x #f])
                                              (+ x x))))))

(displayln "Typecheck tests finished")
