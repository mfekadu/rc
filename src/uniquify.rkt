#!/usr/bin/env racket
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

; uniquify-exp
; given an R1 expr and an association list
; output an R1 expr with all unique variables
(define (uniquify-exp e alist)
  (match e
    ; Look up the symbol in the alist. If it exists, return the mapping in the alist.
    ; Otherwise, throw an error because the symbol wasn't defined
    [(? symbol? s) (alist-get s alist)]
    [(? integer?) e]
    [`(let ([,var ,val]) ,body)
     ; uniquify val expr with old alist
     (define uniquified-val (uniquify-exp val alist))

     ; update the alist to reflect the newly defined var
     ; if it exists, update the mapping
     (define new-alist (alist-update var alist))

     (define new-var (alist-get var new-alist))

     ; then evaluate the body with the updated alist
     (define uniquified-body (uniquify-exp body new-alist))

     ; then return the whole uniquified let expr
     `(let ([,new-var ,uniquified-val]) ,uniquified-body)]
    [`(,op ,es ...)
     `(,op ,@(for/list ([e es]) (uniquify-exp e alist)))]
    [_ (error 'uniquify-exp "Malformed expression: ~s" e)]))

; given an R1 program
; output an R1 program that has entirely unique variables
; e.g. (let ([x 32]) (+ (let ([x 10]) x) x)) >> (let ([x.1 32]) (+ (let ([x.2 10]) x.2) x.1))
; such that output of both is 42.
(define (uniquify p)
  (match p
    [`(program ,info (,label ,(? list? e)))
     ; Strictly follow the R1 grammar for now because '(start (+ 2 2)) is not an expr
     ; Feel free to refactor for R2.
     (error 'uniquify "Malformed expr in program: ~s" p)]
    [`(program ,info ,e)
     `(program ,info ,(uniquify-exp e '()))]
    [_ (error 'uniquify "Malformed program: ~s" p)]))
