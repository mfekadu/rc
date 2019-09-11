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

(define instrs3 '((label start)
                  (movq (int 1) (var v))     ; 2
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
                    (set)
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
(define given4-blocks `(block ,live-list3
                              (label start)
                              (movq (int 1) (var v))     ; 2
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
                              (jmp conclusion)))

(define given4 `(program () (,given4-blocks)))
(define expected-graph4 `((z ,(set) ,(set 't.1 'y 'w))
                          (t.1 ,(set) ,(set 'z))
                          (y ,(set) ,(set 'z 'x 'w))
                          (w ,(set 'rax 'rcx 'rdx 'rsi 'rdi 'r8 'r9 'r10 'r11) ,(set 'z 'y 'x 'v))
                          (x ,(set) ,(set 'y 'w))
                          (v ,(set 'rax 'rcx 'rdx 'rsi 'rdi 'r8 'r9 'r10 'r11) ,(set 'w))))
(define expect4 `(program ((locals ()) (conflicts ,expected-graph4)) (,given4-blocks)))
(check-equal? (make-interference given4) expect4)

(define given5-blocks `((block 
                          (,(set) ,(set) ,(set) ,(set))
                          (label block176) (movq (int 0) (reg rax)) (jmp conclusion))
                        (block
                          (,(set 'b 'c) ,(set 'b 'c) ,(set) ,(set))
                          (label block178) (addq (var b) (var c)) (jmp block176))
                        (block
                          (,(set 'a) ,(set 'a) ,(set 'a 'd) ,(set) ,(set))
                          (label block177) (movq (int 4) (var d)) (addq (var d) (var a)) (jmp block176))
                        (block
                          (,(set) ,(set) ,(set) ,(set 'a) ,(set 'a 'b) ,(set 'a 'b 'c) ,(set 'a 'b 'c) ,(set 'a 'b 'c))
                          (label start) (movq (int 1) (var a)) (movq (int 2) (var b)) (movq (int 3) (var c))
                          (cmpq (int 1) (int 1)) (jmp-if e block177) (jmp block178))))
(define given5 `(program () ,given5-blocks))
                   
(define expected-graph5 `((b ,(set) ,(set 'a 'c))
                          (c ,(set) ,(set 'b 'a))
                          (a ,(set) ,(set 'b 'c 'd))
                          (d ,(set) ,(set 'a))))
(check-equal? (make-interference given5) `(program ((locals ()) (conflicts ,expected-graph5)) ,given5-blocks))





; more tests

; some inputs
(define b1 '((label block2861) (movq (int 42) (reg rax)) (jmp conclusion)))
(define b1-las (map (λ (x) (set)) b1))
(define b2 '((label block2862) (movq (int 44) (reg rax)) (jmp conclusion)))
(define b2-las (map (λ (x) (set)) b2))
(define b3 '((label block2863) (cmpq (var complex-if2860) (int 1)) (jmp-if e block2861) (jmp block2862)))
(define b3-las (map (λ (x) (set)) b3))
(define b4 '((label block2864) (movq (int 0) (var complex-if2860)) (jmp block2863)))
(define b4-las (map (λ (x) (set)) b4))
(define b5 '((label block2865) (cmpq (int 2) (int 1)) (set e (byte-reg al)) (movzbq (byte-reg al) (var complex-op2859)) (cmpq (int 0) (var complex-op2859)) (set e (byte-reg al)) (movzbq (byte-reg al) (var complex-if2860)) (jmp block2863)))
(define b5-las (map (λ (x) (set)) b5))
(define b6 '((label start) (cmpq (int 2) (int 1)) (set l (byte-reg al)) (movzbq (byte-reg al) (var complex-op2858)) (cmpq (var complex-op2858) (int 1)) (jmp-if e block2864) (jmp block2865)))
(define b6-las (map (λ (x) (set)) b6))

; expect???????

(displayln "make interference tests done")
