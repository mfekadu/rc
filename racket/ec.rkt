#!/usr/bin/racket
#lang racket
(require rackunit)
(require racket/contract)
(require "test-helpers.rkt") ; for check-fail and check-fail-with-name
(provide explicate-control)

; function to break off the program part of the C0 syntax
(define (explicate-control prog) 
  (match prog
    [`(program ,locals (,label ,expr)) 
      `(program ,locals (,label ,(ec-tail expr)))]
    [_ (error "malformed program input to ec-prog")]))

(define (ec-tail e)
  (match e
    ; when given something simple, ec-tail makes returns
    [(? symbol?)               `(return ,e)]
    [(? integer?)              `(return ,e)]
    ; when an expr with assignments, it relies on it's friend ec-assign
    [`(let ([,var ,val]) ,body) (ec-assign val var (ec-tail body))]
    ; operations are just operations
    ; operation arm at bottom to allow for matching let first
    [`(,op ,args ...) `(return ,e)]))

(define (ec-assign val var tail)
  (match val
    ; when given simple cases, make a C0 that looks like `var = val; `tail;``
    [(? symbol? s)    `(seq (assign ,var ,s) ,tail)]
    [(? integer?)     `(seq (assign ,var ,val) ,tail)]
    [`(read)          `(seq (assign ,var ,val) ,tail)]
    ; when given 
    [`(let ([,new_var ,val]) ,body)
     (define new_tail (ec-assign body var tail)) 
     (ec-assign val new_var new_tail)]
    ; operations are similar to the atomic cases
    [`(,op ,args ...) `(seq (assign ,var ,val) ,tail)]))

; atomic test cases
(check-equal? (ec-tail 3) (list 'return 3))

; simple let case
(check-equal? (ec-tail '(let ([x 2]) x)) '(seq (assign x 2) (return x)))

; complex let case
(define complex_let
  '(let ([x 2])
     (let ([y 1]) (+ x y))))
(check-equal? (ec-tail complex_let)
              '(seq (assign x 2)
                    (seq (assign y 1)
                         (return (+ x y)))))

(check-equal? (ec-tail '(let ([x 2]) x)) '(seq (assign x 2) (return x)))

; complex let case
(define complex_nested_val_let
  '(let ([x (let ([z 6]) z)]) (+ x 1)))
(check-equal? (ec-tail complex_nested_val_let)
              '(seq (assign z 6)
                    (seq (assign x z)
                         (return (+ x 1)))))


; complex let case with let in the body
(define complex_nested_val_and_body_let
  '(let ([x (let ([z 6]) (let ([y (+ z 1)]) (+ y 1)))]) (+ x 1)))
(check-equal? (ec-tail complex_nested_val_and_body_let)
              '(seq (assign z 6)
                    (seq (assign y (+ z 1))
                         (seq (assign x (+ y 1))
                              (return (+ x 1))))))



; simple read test case
(check-equal? (ec-tail '(let ([x (read)]) x)) '(seq (assign x (read)) (return x)))


; the one case we are not handling
(define let_in_the_body_for_ec-tail_not_ec-assign
  '(let ([x 1]) (let ([y 2]) (+ x y))))

(check-equal?
 (ec-tail let_in_the_body_for_ec-tail_not_ec-assign)
 '(seq (assign x 1) (seq (assign y 2) (return (+ x y)))))


; one more super weird case
(define complex_nested_val_and_body_let_and_the_outer_body_has_a_let
  '(let ([x (let ([z 6]) (let ([y (+ z 1)]) (+ y 1)))]) (let ([foo 42]) (+ foo x))))
(check-equal? (ec-tail complex_nested_val_and_body_let_and_the_outer_body_has_a_let)
              '(seq (assign z 6)
                    (seq (assign y (+ z 1))
                         (seq (assign x (+ y 1))
                              (seq (assign foo 42)
                                   (return (+ foo x)))))))


; EC TAIL TESTS
; simple var case
(check-equal? (ec-tail 'x) '(return x))

; simple addition
(check-equal? (ec-tail '(+ 1 1)) '(return (+ 1 1)))
(check-equal? (ec-tail '(+ fizz buzz)) '(return (+ fizz buzz)))


; simple negation
(check-equal? (ec-tail '(- 1)) '(return (- 1)))
(check-equal? (ec-tail '(- foo)) '(return (- foo)))

; testing explicate-control
(check-equal? (explicate-control `(program () (start (- 1)))) `(program () (start (return (- 1)))))
(check-equal? (explicate-control `(program () (start (let ([x 2]) (let ([y 1]) (+ x y))))))
                                 `(program () (start (seq (assign x 2)
                                                     (seq (assign y 1)
                                                     (return (+ x y)))))))

; test bad prog
(check-fail (λ () (explicate-control 'foo)))

(displayln "tests finished running")
