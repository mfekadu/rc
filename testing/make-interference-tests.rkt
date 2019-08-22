#!/usr/local/bin/racket
#lang racket
(require rackunit)
(require racket/contract)
(require "utilities.rkt") ; for check-fail and check-fail-with-name
(require "../src/make-interference.rkt")

(define instrs1 '((movq (int 1) (var v))
                  (movq (int 46) (var w))
                  (movq (var v) (var x))
                  (addq (int 7) (var x))))

(define live-list1 (list
                     (set)
                     (set 'v)
                     (set 'v 'w)
                     (set 'w 'x)
                     (set 'w 'x)))
(define expect1 `((w ,(set) ,(set 'x 'v))     ; w interferes with v on instr 2
                  (x ,(set) ,(set 'w))       ; x interferes with w on instrs 3 and 4
                  (v ,(set) ,(set 'w))))

(check-equal? (interference-from-live live-list1 instrs1 '()) expect1)
; full example from pg 40 except the register parts that idk how to handle
(define instrs2 '((movq (int 1) (var v))     ; 2
                  (movq (int 46) (var w))    ; 3
                  (movq (var v) (var x))     ; 4
                  (addq (int 7) (var x))     ; 5
                  (movq (var x) (var y))     ; 6
                  (addq (int 4) (var y))     ; 7
                  (movq (var x) (var z))     ; 8
                  (addq (var w) (var z))     ; 9
                  (movq (var y) (var t.1))   ; 10
                  (negq (var t.1))           ; 11
                  (movq (var z) (reg rax))   ; 12
                  (addq (var t.1) (reg rax)) ; 13
                  (jmp conclusion)))         ; 14

(define live-list2 (list
                    (set)          ; 1
                    (set 'v)       ; 2
                    (set 'v 'w)    ; 3
                    (set 'w 'x)    ; 4
                    (set 'w 'x)    ; 5
                    (set 'w 'x 'y) ; 6
                    (set 'w 'x 'y) ; 7
                    (set 'w 'y 'z) ; 8
                    (set 'y 'z)    ; 9
                    (set 'z 't.1)  ; 10
                    (set 'z 't.1)  ; 11
                    (set 't.1)     ; 12
                    (set)          ; 13
                    (set)))        ; 14

(define expect2 `((z ,(set) ,(set 't.1 'y 'w))
                  (t.1 ,(set) ,(set 'z))
                  (w ,(set) ,(set 'z 'y 'x 'v))
                  (y ,(set) ,(set 'z 'x 'w))
                  (x ,(set) ,(set 'y 'w))
                  (v ,(set) ,(set 'w))))
(check-equal? (interference-from-live live-list2 instrs2 '()) expect2)

(define instrs3 '((movq (int 1) (var v))     ; 2
                  (movq (int 46) (var w))    ; 3
                  (callq read_int)           ; callq instr
                  (movq (var v) (var x))     ; 4
                  (addq (int 7) (var x))     ; 5
                  (movq (var x) (var y))     ; 6
                  (addq (int 4) (var y))     ; 7
                  (movq (var x) (var z))     ; 8
                  (addq (var w) (var z))     ; 9
                  (movq (var y) (var t.1))   ; 10
                  (negq (var t.1))           ; 11
                  (movq (var z) (reg rax))   ; 12
                  (addq (var t.1) (reg rax)) ; 13
                  (jmp conclusion)))         ; 14

(define live-list3 (list
                    (set)          ; 1
                    (set 'v)       ; 2
                    (set 'v 'w)    ; 3
                    (set 'v 'w)    ; callq instr
                    (set 'w 'x)    ; 4
                    (set 'w 'x)    ; 5
                    (set 'w 'x 'y) ; 6
                    (set 'w 'x 'y) ; 7
                    (set 'w 'y 'z) ; 8
                    (set 'y 'z)    ; 9
                    (set 'z 't.1)  ; 10
                    (set 'z 't.1)  ; 11
                    (set 't.1)     ; 12
                    (set)          ; 13
                    (set)))        ; 14

(define expect3 `((z ,(set) ,(set 't.1 'y 'w))
                  (t.1 ,(set) ,(set 'z))
                  (w ,(set 'rax 'rcx 'rdx 'rsi 'rdi 'r8 'r9 'r10 'r11) ,(set 'z 'y 'x 'v))
                  (y ,(set) ,(set 'z 'x 'w))
                  (x ,(set) ,(set 'y 'w))
                  (v ,(set 'rax 'rcx 'rdx 'rsi 'rdi 'r8 'r9 'r10 'r11) ,(set 'w))))

(check-equal? (interference-from-live live-list3 instrs3 '()) expect3)

; full make-intereference tests
(define given4 `(program () (start (block ,live-list3 ,instrs3))))
(define expected-graph4 `((z ,(set) ,(set 't.1 'y 'w))
                          (t.1 ,(set) ,(set 'z))
                          (y ,(set) ,(set 'z 'x 'w))
                          (w ,(set 'rax 'rcx 'rdx 'rsi 'rdi 'r8 'r9 'r10 'r11) ,(set 'z 'y 'x 'v))
                          (x ,(set) ,(set 'y 'w))
                          (v ,(set 'rax 'rcx 'rdx 'rsi 'rdi 'r8 'r9 'r10 'r11) ,(set 'w))))
(define expect4 `(program () (start (block ,expected-graph4 ,instrs3))))
(check-equal? (make-interference given4) expect4)

(displayln "make interference tests done")