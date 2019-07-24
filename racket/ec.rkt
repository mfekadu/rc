#lang racket
(require rackunit)
(require racket/contract)

; function to break off the program part of the C0 syntax
(define (ec_prog p) p)

(define (ec_tail e)
  (match e
    [(list 'let bind body)

     ; TODO: get the fancy cond in rco.rkt in the same let match arm of rco_exp
     ; TODO: functionalize this fancy cond
     (cond [(and (list? bind)
                 (list? (first bind)))]
           [else (error "expect let_expr in form (let ([var expr]) expr)")])

     ; we can match first of bind because of the above conditional
     (define output (ec_assign bind (ec_tail body)))
     (displayln output)
     output]
    [_ `(return ,e)]))

(define (ec_assign e tail)
  (match e
    [(list bind)
     (define stmt `(assign ,(first bind) ,(first (rest bind))))
     (define seq `(seq ,stmt ,tail))
     seq]
    [_ (list e tail)]))

; atomic test cases

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