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
(check-equal? (typecheck-exp '() '(not #f)) 'Boolean)

(check-equal? (typecheck-exp '() '(- 2)) 'Integer)
(check-equal? (typecheck-exp '() '(+ 2 2)) 'Integer)
(check-equal? (typecheck-exp '() '(- 2 2)) 'Integer)
(check-equal? (typecheck-exp '() '(- 2 (read))) 'Integer)

(check-equal? (typecheck-exp '() '(eq? 3 4)) 'Boolean)
(check-equal? (typecheck-exp '() '(eq? #f #t)) 'Boolean)

; stuff with let
(check-equal? (typecheck-exp '() '(let ([x 2]) x)) 'Integer)
(check-equal? (typecheck-exp '() '(let ([x (+ 4 1)]) (eq? x 5))) 'Boolean)

; comparison ops
(check-equal? (typecheck-exp '() '(< 1 2)) 'Boolean)
(check-equal? (typecheck-exp '() '(> 1 2)) 'Boolean)

; comparison ops
(check-equal? (typecheck-exp '() '(or #f #t)) 'Boolean)
(check-equal? (typecheck-exp '() '(and #f #t)) 'Boolean)

; if case
(check-equal? (typecheck-exp '() '(if (> 3 4) (+ 2 1) (- 1))) 'Integer)
(check-equal? (typecheck-exp '() '(if (> 3 4) (> 2 1) (< 1 4))) 'Boolean)

; more complex case
(define given1 '(let ([x (if #t (+ 1 2) (- 3))])
                  (eq? (if (> x 4) (and #t #t) (not #f)) #t)))

(check-equal? (typecheck-exp '() given1) 'Boolean)

; err cases
(check-fail (lambda () (typecheck-exp '() 'x)))
(check-fail (lambda () (typecheck-exp '() '(+ #f #t))))
(check-fail (lambda () (typecheck-exp '() '(let ([x #f]) (+ x 2)))))
(check-fail (lambda () (typecheck-exp '() '(eq? #f 0))))
(check-fail (lambda () (typecheck-exp '() '(> #f 0))))
(check-fail (lambda () (typecheck-exp '() '(<=> 2 0))))
(check-fail (lambda () (typecheck-exp '() '(- #f))))
(check-fail (lambda () (typecheck-exp '() '(or #f 1))))
(check-fail (lambda () (typecheck-exp '() '(not 1))))
(check-fail (lambda () (typecheck-exp '() '(if 2 #t #f))))
(check-fail (lambda () (typecheck-exp '() '(if #t #f 1))))

; what's going to happen with variable shadowing?
; this is supposed to be called before uniquify
; Apparently this works, but won't there be 2 entries for x in the alist? What does dict-ref do when that happens?
(check-fail (lambda () (typecheck-exp '() '(let ([x (+ 2 2)])
                                             (let ([x #f])
                                              (+ x x))))))

; toplevel typecheck test
(define given1-prog `(program '() ,given1))
(check-equal? (typecheck given1-prog) given1-prog)

(displayln "Typecheck tests finished")
