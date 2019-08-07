#!/usr/bin/racket
#lang racket
(require rackunit)
(require racket/contract)
(require "test-helpers.rkt") ; for check-fail and check-fail-with-name


; https://rosettacode.org/wiki/Repeat_a_string#Racket
; HELPER for repeating a string
(define (string-repeat n str)
  (string-append* (make-list n str)))
(check-equal? (string-repeat 5 "ha") "hahahahaha")

; GLOBAL variable for the system
; Either 'macosx, 'unix, or 'windows
(define SYS (system-type 'os))

; GLOBAL variable to use a consistent indentation
(define INDENT (string-repeat 6 " ")) ; "      "
(define NEWLINE "\n")
(define SPACE " ")



; ==================================================
; ==================================================
; An x86 program equivalent to (+ 10 32).
; 
;       .globl main
; main:
;       movq $10, %rax
;       addq $32, %rax
;       retq
; ==================================================
; ==================================================
; An x86 program equivalent to (+ 52 (- 10)).
; start:
;       movq $10, -8(%rbp)
;       negq -8(%rbp)
;       movq -8(%rbp), %rax
;       addq $52, %rax
;       jmp conclusion
; 
;       .globl main
; main:
;       pushq %rbp
;       movq %rsp, %rbp
;       subq $16, %rsp
;       jmp start
; 
; conclusion:
;       addq $16, %rsp
;       popq %rbp
;       retq
; ==================================================
; ==================================================


; TODO: write a HELPER for jmp conclusion
; it should do like popq retq or something like that.
; and with the label conclusion or something like that...
; see test.s AND test_64bit.s
; JK. print-x86 doesnt care if you have good assembly.. it will just print the text 

; examples of how format works
(format "foo") ; "foo"
(format "foo ~v" 2) ; "foo 2"
; examples of how string-append works
(string-append) ; ""
(string-append "foo") ; "foo"
(string-append "foo" "bar") ; "foobar"


; given a pseudo-x86 program AST
; output the string representation of the x86 syntax
(define (print-x86-prog x)
  (match x
    [`(program ,locals (,label ,instrs))
     (displayln (format "what do I do with the locals? ~v" locals))
     (define label_str (format "~s:" label))
     (string-append INDENT ".global main" NEWLINE
                    label_str NEWLINE
                    (rec-print-x86-instr instrs))]
    [_ (format "~v" x)]))


; given a pseudo-x86 instruction AST list thing
; output the string representation of the x86 syntax
(define (rec-print-x86-instr instrs)
  (cond [(empty? instrs) ""]
        [else (string-append INDENT (print-x86-instr (first instrs)) NEWLINE
                      (rec-print-x86-instr (rest instrs)))]))

; given a SINGLE pseudo-x86 instruction
; output the string representation of the x86 syntax
(define (print-x86-instr x)
  (match x
    [`(,op ,arg1 ,arg2)
     (string-append (format "~s" op) SPACE
                    (print-x86-arg arg1) SPACE
                    (print-x86-arg arg2))]
    [_ (error 'print-x86-instr "bad instruction ~s" x)]))


; given an pseudo-x86 arg return the string representation of it
(define (print-x86-arg a)
  (match a
    [`(int ,n) (format "$~s" n)]
    [`(reg ,r) (format "%~s" r)]
    [`(deref ,reg ,offset) (format "~s(%~s)" offset reg)]
    [_ (error 'print-x86-arg "bad x86 arg ~v" a)]))

; TEST print-x86-prog
(check-true (string? (print-x86-prog '())))
(define instr '((addq (int 2) (deref rbp -8))))
(define expect "      .global main\nstart:\n      addq $2 -8(%rbp)\n")
(check-equal? (print-x86-prog `(program () (start ,instr))) expect)

; TEST print-x86-arg
(check-equal? (print-x86-arg '(int 42)) "$42")
(check-equal? (print-x86-arg '(reg rsp)) "%rsp")
(check-equal? (print-x86-arg '(reg r12)) "%r12")
(check-equal? (print-x86-arg '(deref rbp -8)) "-8(%rbp)")
(check-fail (λ () (print-x86-arg 'foobar)))
(check-fail (λ () (print-x86-arg 'foobar)))

; TEST print-x86-instr
(check-fail (λ () (print-x86-instr 'x)))
(check-equal? (print-x86-instr '(addq (int 42) (reg rax))) "addq $42 %rax")

  