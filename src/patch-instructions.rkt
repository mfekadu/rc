#!/usr/bin/env racket
#lang racket
(require "utilities.rkt")
(provide patch-instructions)
(provide is-mult-8?)
(provide is-neg-mult-8?)
(provide replace-invalid-instrs)

(define (is-neg-mult-8? n) (and (fixnum? n) (negative? n) (= (modulo n 8) 0)))
(define (is-mult-8? n) (and (fixnum? n) (= (modulo n 8) 0)))
; Given an x86 program, break up all instructions of the form
;               op (deref rbp v1) (deref rbp v2)
; into 
;               mov (deref rbp v1) (reg rax)
;               op (deref rbp v2) (reg rax)
;               mov (reg rax) (deref rbp v2)
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
    [`(program ,locals ,(? blocks? blocks))
      (define patched-blocks
        (for/list ([b blocks])
          (match b
            [`(block ,info ,instrs ...)
              `(block ,info ,@(rec-replace-invalid-instrs instrs))]
            [_ (error 'patch-instructions "Bad block ~v " b)])))
        `(program ,locals ,patched-blocks)]
    [_ (error 'patch-instructions "Bad program ~v " x86-prog)]))

(define (rec-replace-invalid-instrs instrs)
  (cond [(empty? instrs) '()]
        [else (append (replace-invalid-instrs (first instrs))
                      (rec-replace-invalid-instrs (rest instrs)))]))

(define (replace-invalid-instrs instrs)
  (match instrs
    [`(,op ,arg1 (int ,b))
      #:when (not (equal? op 'cmpq))
      (error 'replace-invalid-instrs "Second argument of instruction ~v shouldn't be an int" instrs)]
    [`(,op ,arg (deref rbp ,offset2))
     #:when (not (is-neg-mult-8? offset2))
     (error 'replace-invalid-instrs "bad offset: {{ ~v }}" offset2)]
    [`(,op (deref rbp ,offset1) (deref rbp ,offset2))
     #:when (or (not (is-neg-mult-8? offset1)) (not (is-neg-mult-8? offset2)))
     (error 'replace-invalid-instrs "one or more bad offsets: {{ ~v }} OR {{ ~v }}" offset1 offset2)]
    [`(movzbq ,arg1 ,arg2)
      #:when (not (equal? arg1 '(byte-reg al)))
      (error 'replace-invalid-instrs "movzbq must always have (byte-reg al) as first arg ~v" instrs)]
    [`(movq (deref rbp ,offset1) (deref rbp ,offset2))
     `((movq (deref rbp ,offset1) (reg rax))
       (movq (reg rax) (deref rbp ,offset2)))]
    [`(addq (deref rbp ,offset1) (deref rbp ,offset2))
     ;LIST OF INSTRS???
     `((movq (deref rbp ,offset1) (reg rax))
      (addq (deref rbp ,offset2) (reg rax))
      (movq (reg rax) (deref rbp ,offset2)))]
    [`(movzbq (byte-reg al) (deref rbp ,offset))
      `((movzbq (byte-reg al) (reg rax))
        (movq (reg rax) (deref rbp ,offset)))]
    [`(cmpq ,arg1 (int ,i))
      `((movq (int ,i) (reg rax))
        (cmpq ,arg1 (reg rax)))]
    [`(cmpq (deref rbp ,offset1) (deref rbp ,offset2))
      `((movq (deref rbp ,offset1) (reg rax))
        (cmpq (deref rbp ,offset2) (reg rax)))]
    [_ `(,instrs)]))
