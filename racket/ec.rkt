#lang racket
(require rackunit)
(require racket/contract)

; function to break off the program part of the C0 syntax
(define (ec_prog p) p)

(define (ec_tail e)
  (match e
    [(? symbol?) `(return ,e)]
    [(? integer?) `(return ,e)]
    [`(let ([,var ,val]) ,body) ; fancy destructuring syntax
     (define output (ec_assign val var (ec_tail body)))
     (displayln output)
     output]
    [`(,op ,args ...) `(return ,e)]))
;    [(list 'let bind body)
;
;     ; TODO: get the fancy cond in rco.rkt in the same let match arm of rco_exp
;     ; TODO: functionalize this fancy cond
;     (cond [(and (list? bind)
;                 (list? (first bind)))]
;           [else (error "expect let_expr in form (let ([var expr]) expr)")])
;
;     ; we can match first of bind because of the above conditional
;     (define output (ec_assign bind (ec_tail body)))
;     (displayln output)
;     output]
;    [_ `(return ,e)]))

(define (ec_assign val var tail)
  (match val
    [(? symbol?)
     (error "not implemented")]
    [(? integer?)
     `(seq (assign ,var ,val) ,tail)]
    [`(read)
     (error "not implemented")]
    [`(let ([,new_var ,val]) ,body)
     (displayln (list "val" val "var" var "tail" tail "new_var" new_var "body" body))
     (error "not implemented")]
    [`(,op ,args ...)
     (error "not implemented")]))
;    ;[(? symbol?) (your-code-here)]
;    ;[(? integer?) `(seq (assign ,x ,e) ,k)]
;    ;[`(read) (your-code-here)]
;    [(list bind)
;     (define stmt `(assign ,(first bind) ,(first (rest bind))))
;     (define seq `(seq ,stmt ,tail))
;     seq]
;    [_ (list e tail)]))

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