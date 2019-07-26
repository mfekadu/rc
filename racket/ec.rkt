#!/usr/bin/racket
#lang racket
(require rackunit)
(require racket/contract)

; function to break off the program part of the C0 syntax
(define (ec_prog p) p)

(define (ec_tail e)
  (match e
    ; when given something simple, ec_tail makes returns
    [(? symbol?)               `(return ,e)]
    [(? integer?)              `(return ,e)]
    ; when an expr with assignments, it relies on it's friend ec_assign
    [`(let ([,var ,val]) ,body) (ec_assign val var (ec_tail body))]
    ; operations are just operations
    ; operation arm at bottom to allow for matching let first
    [`(,op ,args ...) `(return ,e)]))

(define (ec_assign val var tail)
  (match val
    ; when given simple cases, make a C0 that looks like `var = val; `tail;``
    [(? symbol? s)    `(seq (assign ,var ,s) ,tail)]
    [(? integer?)     `(seq (assign ,var ,val) ,tail)]
    [`(read)          `(seq (assign ,var ,val) ,tail)]
    ; when given 
    [`(let ([,new_var ,val]) ,body)
     (define new_tail (ec_assign body var tail)) 
     (ec_assign val new_var new_tail)]
    ; operations are similar to the atomic cases
    [`(,op ,args ...) `(seq (assign ,var ,val) ,tail)]))

; atomic test cases
(check-equal? (ec_tail 3) (list 'return 3))

; simple let case
(check-equal? (ec_tail '(let ([x 2]) x)) '(seq (assign x 2) (return x)))

; complex let case
(define complex_let
  '(let ([x 2])
     (let ([y 1]) (+ x y))))
(check-equal? (ec_tail complex_let)
              '(seq (assign x 2)
                    (seq (assign y 1)
                         (return (+ x y)))))

(check-equal? (ec_tail '(let ([x 2]) x)) '(seq (assign x 2) (return x)))



; complex let case
(define complex_nested_val_let
  '(let ([x (let ([z 6]) z)]) (+ x 1)))
(check-equal? (ec_tail complex_nested_val_let)
              '(seq (assign z 6)
                    (seq (assign x z)
                         (return (+ x 1)))))


; complex let case with let in the body
(define complex_nested_val_and_body_let
  '(let ([x (let ([z 6]) (let ([y (+ z 1)]) (+ y 1)))]) (+ x 1)))
(check-equal? (ec_tail complex_nested_val_and_body_let)
              '(seq (assign z 6)
                    (seq (assign y (+ z 1))
                         (seq (assign x (+ y 1))
                              (return (+ x 1))))))



; simple read test case
(check-equal? (ec_tail '(let ([x (read)]) x)) '(seq (assign x (read)) (return x)))



; the one case we are not handling
(define let_in_the_body_for_ec_tail_not_ec_assign
  '(let ([x 1]) (let ([y 2]) (+ x y))))

(check-equal?
 (ec_tail let_in_the_body_for_ec_tail_not_ec_assign)
 '(seq (assign x 1) (seq (assign y 2) (return (+ x y)))))


; one more super weird case
(define complex_nested_val_and_body_let_and_the_outer_body_has_a_let
  '(let ([x (let ([z 6]) (let ([y (+ z 1)]) (+ y 1)))]) (let ([foo 42]) (+ foo x))))
(check-equal? (ec_tail complex_nested_val_and_body_let_and_the_outer_body_has_a_let)
              '(seq (assign z 6)
                    (seq (assign y (+ z 1))
                         (seq (assign x (+ y 1))
                              (seq (assign foo 42)
                                   (return (+ foo x)))))))


; EC TAIL TESTS
; simple var case
(check-equal? (ec_tail 'x) '(return x))

; simple addition
(check-equal? (ec_tail '(+ 1 1)) '(return (+ 1 1)))
(check-equal? (ec_tail '(+ fizz buzz)) '(return (+ fizz buzz)))


; simple negation
(check-equal? (ec_tail '(- 1)) '(return (- 1)))
(check-equal? (ec_tail '(- foo)) '(return (- foo)))
(displayln "tests pass")
