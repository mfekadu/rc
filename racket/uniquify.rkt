#!/usr/bin/racket
#lang racket
(require rackunit)
(require racket/contract)
(require "test-helpers.rkt") ; for check-fail and check-fail-with-name

(provide uniquify)

; looks up s in alist. Returns the mapped symbol if it exists, otherwise throws an error
(define (alist-get s alist)
  (define entry (assoc s alist))
  (if (eq? entry #f)
    (error "Syntax error: symbol ~s is undefined" s)
    (cadr entry)))

; Generates a new symbol for s. If it exists in the alist, update it, otherwise make a new entry
; in the alist.
(define (alist-update s alist)
  (define new-sym (gensym s))
  (define entry (assoc s alist))
  (if (eq? entry #f)
    ; if s not in alist, just append the new mapping
    (cons `(,s ,new-sym) alist)
    ; if s in alist, remove the old mapping and append the new mapping
    (cons `(,s ,new-sym) (remove entry alist))))

(define (uniquify-exp alist)
  (lambda (e)
    (match e
      ; Look up the symbol in the alist. If it exists, return the mapping in the alist.
      ; Otherwise, throw an error because the symbol wasn't defined 
      [(? symbol? s) (alist-get s alist)]
      [(? integer?) e]
      [`(let ([,var ,val]) ,body) 
        ; uniquify val expr with old alist
        (define uniquified-val ((uniquify-exp alist) val))

        ; update the alist to reflect the newly defined var
        ; if it exists, update the mapping
        (define new-alist (alist-update var alist))

        (define new-var (alist-get var new-alist))

        ; then evaluate the body with the updated alist
        (define uniquified-body ((uniquify-exp new-alist) body))

        ; then return the whole uniquified let expr
        `(let ([,new-var ,uniquified-val]) ,uniquified-body)]
      [`(,op ,es ...)
       `(,op ,@(for/list ([e es]) ((uniquify-exp alist) e)))]
      [_ (error "Malformed expression given to uniquify-exp: ~s" e)])))

(define uniquify
  (lambda (e)
    (match e
      [`(program ,info (,label ,e))
       `(program ,info (,label ,((uniquify-exp '()) e)))]
      [_ (error "Malformed program given to uniquify: ~s" e)])))

; tests for uniquify-exp
(define uniquify-exp-func (uniquify-exp '()))

(define given1 '(+ 2 2))
(define expect1 '(+ 2 2))
(check-equal? (uniquify-exp-func given1) expect1)

; renaming should work
(define given2 '(let ([x 2]) (+ 2 x)))
(check-match (uniquify-exp-func given2)
              `(let ([,(? symbol? s) 2]) (+ 2 ,s)))

(define given3 '(let ([x 1]) (let ([x x]) (+ x x))))
(check-match (uniquify-exp-func given3)
              `(let ([,(? symbol? s1) 1]) 
                 (let ([,(? symbol? s2) ,s1])
                   (+ ,s2 ,s2))))

; should fail since x is not defined
(define given4 '(+ x 2))
(check-fail (lambda () (uniquify-exp-func given4)))

(define given5 '(let ([x 5]) (+ y 3)))
(check-fail (lambda () (uniquify-exp-func given5)))

(displayln "tests pass")
