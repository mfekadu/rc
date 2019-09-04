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
    [`(- ,e1 ,e2) `(+ ,e1 (- ,e2))]
    [`(and ,e1 ,e2) `(if (eq? ,e1 #t) (eq? ,e2 #t) #f)]
    [`(or ,e1 ,e2) `(if (eq? ,e1 #f) (eq? ,e2 #t) #t)]
    [`(<= ,e1 ,e2) `(if (< ,e1 ,e2) #t (eq? ,e1 ,e2))]
    [`(> ,e1 ,e2) `(if (< ,e1 ,e2) #f (not (eq? ,e1 ,e2)))]
    [`(>= ,e1 ,e2) `(not (< ,e1 ,e2))]
    [`(let ([,var ,val]) ,body)
      `(let ([,var ,(shrink-exp val)]) ,(shrink-exp body))]
    [`(,op ,args ...)
      (cons op (for/list ([arg args]) (shrink-exp arg)))]))
