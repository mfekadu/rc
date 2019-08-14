#!/usr/local/bin/racket
#lang racket
(provide print-x86)

; for testing
(provide print-x86-arg)
(provide print-x86-instr)
(provide MAIN)


; https://rosettacode.org/wiki/Repeat_a_string#Racket
; HELPER for repeating a string
(define (string-repeat n str)
  (string-append* (make-list n str)))

; GLOBAL variable for the system
; Either 'macosx, 'unix, or 'windows
(define SYS (system-type 'os))
(define MAIN (cond 
               [(equal? SYS 'macosx) "_main"]
               [(equal? SYS 'unix) "main"]
               [else (error "windows not supported")]))

; GLOBAL variable to use a consistent indentation
(define INDENT (string-repeat 6 " ")) ; "      "
(define NEWLINE "\n")
(define COMMA ",")
(define SPACE " ")

; ==================================================
; ==================================================
; An x86 program equivalent to (+ 10 32).
; 
;       .globl main
; main:
;       movq $10, %rax
;       addq $32, %rax
;       retq # MUST INCLUDE ELSE SEG FAULT! 
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


; given a pseudo-x86 program AST
; output the string representation of the x86 syntax
(define (print-x86 x)
  (match x
    [`(program ,locals (,label ,instrs))
     (define label_str (string-append MAIN ":")) ; TODO: consider prologue? like "start:"
     (string-append INDENT ".global " MAIN NEWLINE
                    label_str NEWLINE
                    INDENT "movq %rsp, %rbp" NEWLINE
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
                    (print-x86-arg arg1) COMMA SPACE
                    (print-x86-arg arg2))]
    [`(jmp ,label) "retq"]
    [`(,op ,arg)
        (string-append (format "~s" op) SPACE
                    (print-x86-arg arg) SPACE)]
    [_ (error 'print-x86-instr "bad instruction ~s" x)]))


; given an pseudo-x86 arg return the string representation of it
(define (print-x86-arg a)
  (match a
    [`(int ,n) (format "$~s" n)]
    [`(reg ,r) (format "%~s" r)]
    [`(deref ,reg ,offset) (format "~s(%~s)" offset reg)]
    [_ (error 'print-x86-arg "bad x86 arg ~v" a)]))
