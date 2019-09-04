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
    [`(- ,e1 ,e2)
     (define e1-s (shrink-exp e1))
     (define e2-s (shrink-exp e2))
     `(+ ,e1-s (- ,e2-s))]

    [`(and ,e1 ,e2)
     (define e1-s (shrink-exp e1))
     (define e2-s (shrink-exp e2))
     `(if (eq? ,e1-s #t) (eq? ,e2-s #t) #f)]

    [`(or ,e1 ,e2)
     (define e1-s (shrink-exp e1))
     (define e2-s (shrink-exp e2))
     `(if (eq? ,e1-s #f) (eq? ,e2-s #t) #t)]

    [`(<= ,e1 ,e2)
     (define e1-s (shrink-exp e1))
     (define e2-s (shrink-exp e2))
     `(if (< ,e1-s ,e2-s) #t (eq? ,e1-s ,e2-s))]

    [`(> ,e1 ,e2)
     (define e1-s (shrink-exp e1))
     (define e2-s (shrink-exp e2))
     `(if (< ,e1-s ,e2-s) #f (not (eq? ,e1-s ,e2-s)))]

    [`(>= ,e1 ,e2)
     (define e1-s (shrink-exp e1))
     (define e2-s (shrink-exp e2))
     `(not (< ,e1-s ,e2-s))]

    [`(let ([,var ,val]) ,body)
      `(let ([,var ,(shrink-exp val)]) ,(shrink-exp body))]
    [`(,op ,args ...)
      (cons op (for/list ([arg args]) (shrink-exp arg)))]))
