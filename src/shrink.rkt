#!/usr/bin/env racket
#lang racket
(provide shrink-exp)
(provide shrink)

(define (shrink program)
  (match program
    [`(program ,info ,body)
     `(program ,info ,(shrink-exp body))]
    [_ (error 'shrink "Malformed program input to typecheck")]))

(define int-or-sym?
  (lambda (x) (or (symbol? x) (fixnum? x))))

(define bool-or-sym?
  (lambda (x) (or (symbol? x) (boolean? x))))

(define (shrink-exp expr)
  (match expr
    [(or (? symbol?) (? fixnum?) (? boolean?) `(read)) expr]
    [`(- ,(? int-or-sym? e1) ,(? int-or-sym? e2)) `(+ ,e1 (- ,e2))]
    [`(and ,(? bool-or-sym? e1) ,(? bool-or-sym? e2)) `(if (eq? ,e1 #t) (eq? ,e2 #t) #f)]
    [`(or ,(? bool-or-sym? e1) ,(? bool-or-sym? e2)) `(if (eq? ,e1 #f) (eq? ,e2 #t) #t)]
    [`(<= ,(? int-or-sym? e1) ,(? int-or-sym? e2)) `(if (< ,e1 ,e2) #t (eq? ,e1 ,e2))]
    [`(> ,(? int-or-sym? e1) ,(? int-or-sym? e2)) `(if (< ,e1 ,e2) #f (not (eq? ,e1 ,e2)))]
    [`(>= ,(? int-or-sym? e1) ,(? int-or-sym? e2)) `(not (< ,e1 ,e2))]
    [`(let ([,var ,val]) ,body)
      `(let ([,var ,(shrink-exp val)]) ,(shrink-exp body))]
    [`(,op ,args ...)
      (cons op (for/list ([arg args]) (shrink-exp arg)))]))
