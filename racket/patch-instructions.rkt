#!/usr/bin/racket
#lang racket
(require rackunit)
(require racket/contract)
(require "test-helpers.rkt") ; for check-fail and check-fail-with-name
(provide patch-instructions)

; Given an x86 program, break up all instructions of the form
;               op (deref rbp v1) (deref rbp v2)
; into 
;               mov (deref rbp v1) (reg rax)
;               op (deref rbp v2) (reg rax)
;               mov (reg rax) (deref rbp v1)
;
; TODO There's a potential OPTIMIZATION here, e.g.:
;     addq (deref rbp -8) (deref rbp -16)
;     addq (deref rbp -8) (deref rbp -24)
; Would become (unoptimized):
;     mov (deref rbp -8) (reg rax)
;     addq (deref rbp -16) (reg rax)
;     mov (reg rax) (deref rbp -8)
;     mov (deref rbp -8) (reg rax)
;     addq (deref rbp -24) (reg rax)
;     mov (reg rax) (deref rbp -8)
(define (patch-instructions x86-prog)
  (match x86-prog 
    [`(program ,locals (,label ,instrs))
     ; instrs is a list of instructions
      `(program ,locals (,label ,(rec-replace-invalid-instrs instrs)))]
    [_ (error "Malformed input program to patch-instructions ~s " x86-prog)]))

(define (rec-replace-invalid-instrs instrs)
  (cond [(empty? instrs) '()]
        [else (append (replace-invalid-instrs (first instrs))
                      (rec-replace-invalid-instrs (rest instrs)))]))


; because every offset must only be a number that is a multiple of 8
(define (is-mult-8? n) (and (fixnum? n) (= (modulo n 8) 0)))
(check-true (is-mult-8? 8))
(check-true (is-mult-8? 0))
(check-true (is-mult-8? -16))
(check-false (is-mult-8? -15))
(check-false (is-mult-8? 'foo))

; also because every offset must only be a negative number
(define (is-neg-mult-8? n) (and (fixnum? n) (negative? n) (= (modulo n 8) 0)))
(check-true (is-neg-mult-8? -8))
(check-false (is-neg-mult-8? 0))
(check-false (is-neg-mult-8? 16))
(check-false (is-neg-mult-8? -15))
(check-false (is-neg-mult-8? 'foo))

(define (replace-invalid-instrs instrs)
  (match instrs
    [`(,op ,arg (deref rbp ,offset2))
     #:when (not (is-neg-mult-8? offset2))
     (error 'replace-invalid-instrs "bad offset: {{ ~v }}" offset2)]
    [`(,op (deref rbp ,offset1) (deref rbp ,offset2))
     #:when (or (not (is-neg-mult-8? offset1)) (not (is-neg-mult-8? offset2)))
     (error 'replace-invalid-instrs "one or more bad offsets: {{ ~v }} OR {{ ~v }}" offset1 offset2)]
    [`(movq (deref rbp ,offset1) (deref rbp ,offset2))
     `((movq (deref rbp ,offset1) (reg rax))
       (movq (reg rax) (deref rbp ,offset2)))]
    [`(,op (deref rbp ,offset1) (deref rbp ,offset2))
     ;LIST OF INSTRS???
     `((movq (deref rbp ,offset1) (reg rax))
      (addq (deref rbp ,offset2) (reg rax))
      (movq (reg rax) (deref rbp ,offset1)))]
    [_ `(,instrs)]))


(define given_bad_offset '(addq (deref rbp v1) (deref rbp v2)))
(check-fail (λ () (replace-invalid-instrs given_bad_offset)))

(define given_bad_offset2 '(addq (deref rbp -8) (deref rbp 16)))
(check-fail (λ () (replace-invalid-instrs given_bad_offset2)))

(define given_bad_offset3 '(addq (int 2) (deref rbp v2)))
(check-fail (λ () (replace-invalid-instrs given_bad_offset3)))

(define given_bad_offset4 '(movq (deref rbp 8) (deref rbp -16)))
(check-fail (λ () (replace-invalid-instrs given_bad_offset4)))

(define given '(addq (deref rbp -8) (deref rbp -16)))
(define expect '((movq (deref rbp -8) (reg rax))
                 (addq (deref rbp -16) (reg rax))
                 (movq (reg rax) (deref rbp -8))))
(check-equal? (replace-invalid-instrs given) expect)

(define given2 '(addq (int 2) (deref rbp -8)))
(define expect2 `(,given2))
(check-equal? (replace-invalid-instrs given2) expect2)

(define given3 '(movq (deref rbp -8) (deref rbp -16)))
(define expect3 '((movq (deref rbp -8) (reg rax))
                 (movq (reg rax) (deref rbp -16))))
(check-equal? (replace-invalid-instrs given3) expect3)
