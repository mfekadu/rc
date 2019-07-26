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
    [(? symbol? s)
     ;(displayln (list "val" s))
     `(seq (assign ,var ,s) ,tail)]
     ;(error "not implemented")]
    [(? integer?)
     `(seq (assign ,var ,val) ,tail)]
    [`(read)
     `(seq (assign ,var ,val) ,tail)]
    [`(let ([,new_var ,val]) ,body)
     ; (let ([z 6]) z)
     (displayln (list "val" val "var" var "tail" tail "new_var" new_var "body" body))
     (define new_tail (ec_assign body var tail)) 
     ;(define new_tail `(seq (assign ,var ,new_body) ,tail))
     (ec_assign val new_var new_tail)]
    [`(,op ,args ...)
     `(seq (assign ,var ,val) ,tail)]))
     ;(error "not implemented")]))
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