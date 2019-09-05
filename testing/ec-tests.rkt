#!/usr/bin/env racket
#lang racket
(require rackunit)
(require racket/contract)
(require "utilities.rkt") ; for check-fail and check-fail-with-name
(require "../src/ec.rkt")

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
(define given1 `(program () (- 1)))
(define expect1 `(program () ((start (return (- 1))))))
(check-equal? (explicate-control given1) expect1)
;
(define given2 `(program () (let ([x 2]) (let ([y 1]) (+ x y)))))
(define expect2 `(program
                  ()
                  ((start (seq (assign x 2)
                              (seq (assign y 1)
                                   (return (+ x y))))))))
(check-equal? (explicate-control given2) expect2)

; test bad prog
(check-fail (λ () (explicate-control 'foo)))


; test bad explicate-control inputs
(check-fail (λ () (explicate-control #t)))
(check-fail (λ () (explicate-control explicate-control)))
; R1 does not have labels
(check-fail (λ () (explicate-control '(program () (start (+ 2 2))))))


; TEST explicate-control return
(check-equal? (explicate-control '(program () (+ 2 2)))
                '(program () ((start (return (+ 2 2))))))

; TEST explicate-control assign
(check-equal? (explicate-control '(program () (let [[x 2]] (+ x 2))))
                '(program () ((start (seq (assign x 2) (return (+ x 2)))))))


; R2 Tests
(define given3 `(if #t 1 2))
;(check-match (explicate-control `(program () ,given3)) 
;             (list 'program '() (list [list-no-order 
;                                        `(start (if (eq? #t #t) (goto ,(? symbol? L1)) (goto ,(? symbol? L2))))
;                                        `(,(? symbol? L1) (return 1))
;                                        `(,(? symbol? L2) (return 2))])))

; for now, hard code in the order we expect
(check-match (explicate-control `(program () ,given3)) 
             `(program () ((,(? symbol? L2) (return 2))
                           (,(? symbol? L1) (return 1))
                           (start (if (eq? #t #t) (goto ,(? symbol? L1)) (goto ,(? symbol? L2)))))))

(check-match (ec-tail given3) `(if (eq? #t #t) (goto ,(? symbol? L1)) (goto ,(? symbol? L2))))
;(define given4 `(+ 1 (if #t 1 2)))
;(check-match (explicate-control `(program () ,given4))
;             `(program () ((start ())
;                           (,L1 ())
;                           (,L2 ()))))
;

(define given4 `(let ([x (if #t 1 2)]) (+ x 1)))
(check-match (ec-tail given4) `(if (eq? #t #t) (goto ,(? symbol? L1)) (goto ,(? symbol? L2))))
(check-match (explicate-control `(program () ,given4))
             `(program () ((,(? symbol? L3) (return (+ x 1)))
                           (,(? symbol? L2) (seq (assign x 2) (goto ,L3)))
                           (,(? symbol? L1) (seq (assign x 1) (goto ,L3)))
                           (start (if (eq? #t #t) (goto ,L1) (goto ,L2))))))
(displayln "ec tests finished")
