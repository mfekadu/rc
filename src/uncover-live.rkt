#!/usr/local/bin/racket
#lang racket

(provide uncover-live)

(require rackunit)
(require "../testing/utilities.rkt") ; for check-fail and check-fail-with-name


; **************************************************
; HELPERS
; **************************************************

; compute the set of variables that appear in an argument (of an instruction)
; return a Set of all variables
(define (get-vars instr)
  (match instr
    ; e.g. (addq (var x) (var y))
    [`(,(? symbol? op) (var ,src) (var ,dest)) (set src dest)]
    ; match a var anywhere in the args while ensuring a symbol for opcode
    [(list-rest (? symbol? op) (list-no-order `(var ,v) arg)) (set v)]
    ; match e.g. (negq (var t)) 
    [`(,(? symbol? op) (var ,v)) (set v)]
    ; match e.g. (jmp conclusion)
    [`(,(? symbol? op) ,arg) (set)]
    [`(,(? symbol? op) ,arg1 ,arg2) (set)] ; return empty set if no var
    [_ (error 'get-vars "bad instr: ~v" instr)]))

; compute the variables read by an instruction, which corresponds to the R(k)
; returns a Set of symbols for variables that are read from
(define (get-read-vars instr)
  (match instr
    ; e.g. (negq (var t))
    [`(,(? symbol? op) (var ,v)) (set v)]
    ; e.g. (jmp conclusion)
    [`(,(? symbol? op) ,arg) (set)]
    ; e.g. (addq (var x) (var y)) ; addq is special case, reads both
    [`(addq (var ,src) (var ,dest)) (set src dest)]
    ; e.g. (addq (var t) (reg rax)) ; reads reg too but that's not var
    ; e.g. (addq (int 42) (var y))
    [(list-rest 'addq (list-no-order `(var ,v) arg2)) (set v)]
    ; e.g. (movq (var x) (var y))
    [`(,op (var ,src) (var ,dest)) (set src)]
    ; e.g. (movq (var t) (reg rax))
    [`(,(? symbol? op) (var ,src) ,arg2) (set src)]
    ; e.g. (movq (int 42) (reg rax))
    [`(,(? symbol? op) ,arg1 ,arg2) (set)] ; return empty set if no var
    [_ (error 'get-read-vars "bad instr: ~v" instr)]))

; compute the variables written by an instruction which corresponds to W(k)
; returns a Set of symbols for variables that are written to
(define (get-write-vars instr)
  (match instr
    ; e.g. (negq (var t))
    [`(,(? symbol? op) (var ,v)) (set v)]
    ; e.g. (jmp conclusion)
    [`(,(? symbol? op) ,arg) (set)]
    ; e.g. (addq (var x) (var y))
    ; e.g. (movq (var x) (var y))
    ; e.g. (addq (reg rax) (var t))
    ; e.g. (movq (reg rax) (var t))
    ; e.g. (addq (int 42) (var y))
    ; convension is write only to dest register
    [`(,(? symbol? op) ,arg1 (var ,dest)) (set dest)]
    ; e.g. (addq (int 42) (reg rax))
    ; e.g. (movq (int 42) (reg rax)) ; write reg too but not var
    [`(,(? symbol? op) ,arg1 ,arg2) (set)] ; return empty set if no var
    [_ (error 'get-write-vars "bad instr: ~v" instr)]))


; given a list of instructions
; and an initial live-after set (typically empty)
; returns the list of live-after Set (i.e. the Set datatype in Racket).
(define (get-live-after-sets instrs init-set)
  (cond
    [(not (set? init-set))
     (error 'get-live-after-sets "bad init-set ~v" init-set)])
  (match instrs
    [(? empty? instrs)
     ; there are no variables live after the last instruction
     ; so return a list containing the empty set
     (list (set))]
    [(list (? list? first-instr) rest-instrs ...)
     ; recursively get the Live_after set from bottom to top
     ; at bottom L_after is (list (set))
     (define L_after (get-live-after-sets rest-instrs init-set))
     (define V (get-vars first-instr))
     (define R (get-read-vars first-instr))
     (define W (get-write-vars first-instr))
     (define L_before (set-union (set-subtract (first L_after) W) R))
     ; now return the combined list of sets
     (cons L_before L_after)]
    [_ (error 'get-live-after-sets "bad instrs ~v" instrs)]))

; **************************************************
; UNCOVER-LIVE
; **************************************************

; given an x86_0 program
; computes the live-after sets as described in Ch3 of Siek et. al.
; returns the same program 
; with the live-after sets as a list-of-lists-of-variables
; inside the "block" clause of the x86_0 grammar (Siek et. al. pg 24)
(define (uncover-live p)
  (error 'uncover-live "not yet implemented"))

; **************************************************
; TEST CASES (TODO: move into external file)
; **************************************************


; ==================================================
; TEST get-vars
; ==================================================
; test 1 vars in addq in second arg position
(define given0 '(addq (int 7) (var y)))
(define expect0 (set 'y))
(check-equal? (get-vars given0) expect0)
; test 2 vars in addq
(define given1 '(addq (var x) (var y)))
(define expect1 (set 'x 'y))
(check-equal? (get-vars given1) expect1)
; test 2 vars in movq
(define given2 '(movq (var x) (var y)))
(define expect2 (set 'x 'y))
(check-equal? (get-vars given2) expect2)
; test 1 var in second arg position in any? binary instruction
(define given3 '(movq (int 10) (var y)))
(define expect3 (set 'y))
(check-equal? (get-vars given3) expect3)
; test 1 var in first arg position in any? binary instruction
(define given4 '(movq (var y) (reg rax)))
(define expect4 (set 'y))
(check-equal? (get-vars given4) expect4)
; test 0 var anywhere in any binary instruction
(define given5 '(movq (int 10) (reg rax)))
(define expect5 (set))
(check-equal? (get-vars given5) expect5)
; test 1 var in any unary instruction
(define given6 '(negq (var t)))
(define expect6 (set 't))
(check-equal? (get-vars given6) expect6)
; test 0 var in any unary instruction
(define given7 '(jmp conclusion))
(define expect7 (set))
(check-equal? (get-vars given7) expect7)
; test bad instr
(define given8 '((int 10) (var y) movq))
(check-fail-with-name 'get-vars get-vars given8)

; ==================================================
; TEST get-read-vars
; ==================================================
; test 1 vars in addq in second arg position
(define given0-read '(addq (int 7) (var y)))
(define expect0-read (set 'y))
(check-equal? (get-read-vars given0-read) expect0-read)
; test 2 vars in addq
(define given1-read '(addq (var x) (var y)))
(define expect1-read (set 'x 'y))
(check-equal? (get-read-vars given1-read) expect1-read)
; test 2 vars in movq
(define given2-read '(movq (var x) (var y)))
(define expect2-read (set 'x))
(check-equal? (get-read-vars given2-read) expect2-read)
; test 1 var in second arg position in any? binary instruction
(define given3-read '(movq (int 10) (var y)))
(define expect3-read (set))
(check-equal? (get-read-vars given3-read) expect3-read)
; test 1 var in first arg position in any? binary instruction
(define given4-read '(movq (var y) (reg rax)))
(define expect4-read (set 'y))
(check-equal? (get-read-vars given4-read) expect4-read)
; test 0 var anywhere in any binary instruction
(define given5-read '(movq (int 10) (reg rax)))
(define expect5-read (set))
(check-equal? (get-read-vars given5-read) expect5-read)
; test 1 var in negq
(define given6-read '(negq (var t)))
(define expect6-read (set 't))
(check-equal? (get-read-vars given6-read) expect6-read)
; test 0 var in any unary instruction
(define given7-read '(jmp conclusion))
(define expect7-read (set))
(check-equal? (get-read-vars given7-read) expect7-read)
; test bad instr
(define given8-read '((int 10) (var y) movq))
(check-fail-with-name 'get-read-vars get-read-vars given8-read)

; ==================================================
; TEST get-write-vars
; ==================================================
; test 1 vars in addq in second arg position
(define given0-write '(addq (int 7) (var y)))
(define expect0-write (set 'y))
(check-equal? (get-write-vars given0-write) expect0-write)
; test 2 vars in addq
(define given1-write '(addq (var x) (var y)))
(define expect1-write (set 'y))
(check-equal? (get-write-vars given1-write) expect1-write)
; test 2 vars in movq
(define given2-write '(movq (var x) (var y)))
(define expect2-write (set 'y))
(check-equal? (get-write-vars given2-write) expect2-write)
; test 1 var in second arg position in any? binary instruction
(define given3-write '(movq (int 10) (var y)))
(define expect3-write (set 'y))
(check-equal? (get-write-vars given3-write) expect3-write)
; test 1 var in first arg position in any? binary instruction
(define given4-write '(movq (var y) (reg rax)))
(define expect4-write (set))
(check-equal? (get-write-vars given4-write) expect4-write)
; test 0 var anywhere in any binary instruction
(define given5-write '(movq (int 10) (reg rax)))
(define expect5-write (set))
(check-equal? (get-write-vars given5-write) expect5-write)
; test 1 var in negq
(define given6-write '(negq (var t)))
(define expect6-write (set 't))
(check-equal? (get-write-vars given6-write) expect6-write)
; test 0 var in any unary instruction
(define given7-write '(jmp conclusion))
(define expect7-write (set))
(check-equal? (get-write-vars given7-write) expect7-write)
; test bad instr
(define given8-write '((int 10) (var y) movq))
(check-fail-with-name 'get-write-vars get-write-vars given8-write)

; ==================================================
; TEST get-live-after-sets
; ==================================================
(define given1-glas '((movq (int 1) (var v))     ; 2
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

(define expect1-glas (list
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

(check-equal? (get-live-after-sets given1-glas (set)) expect1-glas)

; ==================================================
; TEST uncover-live
; ==================================================