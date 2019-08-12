#!/usr/local/bin/racket
#lang racket

(provide uniquify)
(provide uniquify-exp)

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
