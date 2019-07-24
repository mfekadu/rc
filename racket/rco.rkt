
#lang racket
(require rackunit)

; https://docs.racket-lang.org/rackunit/api.html#%28def._%28%28lib._rackunit%2Fmain..rkt%29._check-exn%29%29
(define (check-fail thunk)
  (check-exn exn:fail? thunk))

; HELPER for rco_arg
; ask if the rco_arg_output is a cons thing
; meaning rco_arg determined a complex arg
(define (is_complex? rco_arg_output)
  (match rco_arg_output
    [(cons (? symbol? new_sym) (? hash? alist)) #t]
    ; the _ should only occur when rco_arg given a simple arg
    [_ #f]))

; test the helper
(check-true (is_complex? (cons 'tmp1 (hash 'tmp1 '(+ 2 2)))))
(check-false (is_complex? 42))


(define (rco_exp e)
  (displayln (list 'rco_exp_INPUT: e))
  (match e
    [(? number? n) n]
    [(? symbol? s) s]
    ; match the read alone before next list match
    [(list 'read) e]
    [(list other) "panic!"]
    ; match let binding
    [(list 'let bind body)

     (cond [(and (list? bind)
                 (list? (first bind)))]
           [else (error "expect let_expr in form (let ([var expr]) expr)")])

     ; we can match first of bind because of the above conditional
     (match (first bind)
       [(list var val) (println (list "var" var "res" (rco_exp val)))])

     (define res1 (rco_arg body))
     (println (list "res1" res1))
     ;(define res2 (rco_arg n2))
     
     (list 'let bind body)
     ]
    ; match operation and rest of args
    [(list op args ...)
     

     (check-true (<= (length args) 2))

     (print (list "input:" e)) (println (list "args:" args))
     (define simplify (λ (e) (rco_arg e)))

     (println (cons "map!" (map simplify args)))
     ;(for ([i args] [x (in-naturals)]) ; for i in args
     ;  (displayln (list "x" x "i" i)))
     
     ;(define res1 (rco_arg args))

     ; do magic on res1 res2

     ; put op back on the list of args
     (cons op args)
     
      ]
    [_ "panic!"]))

; given expr
; return (expr, alist)
(define (rco_arg e)
  ; the displayln function pretty prints stuff
  (displayln (list 'rco_arg_INPUT: e))
  (match e
    [(? number? n) n]
    [(? symbol? s) s]
    [(list 'let (list (list (? symbol? var) val)) body)
     ; return same let
     ; quasiquote followed by comma is like a sexp destructuring?
     ; weird shit.
     ; test case: (displayln (rco_arg '(let ([x 2]) x)))

     ; if val is simple, then expect unchanged on output
     ; https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._cdr%29%29
     ; car gets the first of a pair
     ; there's weird stuff too like cddr and cdddr and caaar and cddar
     (define val_out (rco_arg val))
     (define body_out (rco_arg body))

     (displayln (list 'val_out val_out))
     (displayln (list 'body_out body_out))

     (define new_val
       (if (is_complex? val_out) ; guard expr
           (car val_out)         ; then expr
           val_out))             ; else expr
     (displayln (list 'new_val new_val))

     (define new_body
       (if (is_complex? body_out) ; guard expr
           (car body_out)         ; then expr
           body_out))             ; else expr
     (displayln (list 'new_body new_body))

     (define tmp_name (gensym "tmp"))

     (define expr_ret (list 'let `(,`[,var ,new_val]) new_body))

     (define alist (hash tmp_name expr_ret))

     (cons tmp_name alist)]
    [(? list? l)

     (define tmp_name (gensym "tmp"))

     (define alist (hash tmp_name (rco_exp l)))

     (cons tmp_name alist)
     ]
    [_ "panic!"]))



; TEST CASES
; ATOMS should stay simple rco_exp
(displayln (list "test..." (check-equal? (rco_exp 2) 2)))
(displayln (list "test..." (check-equal? (rco_exp '+) '+)))
; ATOMS should stay simple rco_arg
(displayln (list "test..." (check-equal? (rco_arg 2) 2)))
(displayln (list "test..." (check-equal? (rco_arg '+) '+)))

; BAD exprs rco_exp
(displayln (list "test..." (check-equal? (rco_exp (list 2)) "panic!")))
(displayln (list "test..." (check-equal? (rco_exp '(x)) "panic!")))
(displayln (list "test..." (check-equal? (rco_exp (list '+)) "panic!")))
(displayln (list "test..." (check-equal? (rco_exp #t) "panic!")))
; BAD exprs rco_arg
(displayln (list "test..." (check-equal? (rco_arg #t) "panic!")))

; SIMPLE exprs SHOULD STAY SIMPLE
(displayln (list "test..." (check-equal? (rco_exp (list '+ 2 2)) '(+ 2 2))))
(displayln (list "test..." (check-equal? (rco_exp '(let ([x 2]) x)) '(let ([x 2]) x))))
(displayln (list "test..." (check-equal? (rco_exp (list 'read)) '(read))))
;;; SIMPLE exprs SHOULD STAY SIMPLE
;;;;; ??? isn't any list passed into rco_arg complex?
;;;;; ... hmmm.... i guess so? idk...
;(check-true (match (rco_arg '(+ 2 2))
;              [(cons new_sym alist)
;               (and (symbol? new_sym)
;                    (hash? alist)
;                    (hash-has-key? alist new_sym))]
;              ; the _ arm should never happen. ignore coverage highlight
;              [_ #f]))
;
;;TODO: this test case way get reconsidered? if a let is complex?
;; because rco_arg is the simplify function
;;(check-equal? (rco_arg '(let (x 2) x)) '(let (x 2) x))
;
;; so is a READ complex?
;; well.. yes if it is inside of something like (+ 2 (read))
;; certainly x86 assembly would prefer to have (let ([tmp (read)]) (+ 2 tmp))
;; well then by that same logic... a let expr should also be considered complex? right?
;; hmmm.... good point.
;;(check-equal? (rco_arg (list 'read)) '(read))



; HELPER FOR rco_arg tests
; given a complex arg
; return true if proper output (pair of symbol and hash where hash has symbol)
; else false (meaning some already simple arg given and rco_arg left it alone)
(define (is_complex_and_has_sym? arg)
  (define output (rco_arg arg))
  (match output
    [(cons new_sym alist)
     (and (symbol? new_sym)
          (hash? alist)
          (hash-has-key? alist new_sym))]
    ; the _ should only occur when rco_arg given a simple arg
    [_
     (displayln (list "is_NOT_complex_and_has_sym bc {{ output: " output " }} {{ arg: " arg " }}"))
     #f]))

; '(+ 2 2) should get simplified
; because if any operation is an arg then it must be simplified
(displayln (list "test..." (check-true (is_complex_and_has_sym? '(+ 2 2)))))
; '2 should be left alone
(displayln (list "test..." (check-false (is_complex_and_has_sym? '2))))
(displayln (list "test..." (check-false (is_complex_and_has_sym? 2))))
(displayln (list "test..." (check-false (is_complex_and_has_sym? '+))))
(displayln (list "test..." (check-false (is_complex_and_has_sym? '-))))
(displayln (list "test..." (check-false (is_complex_and_has_sym? 'read))))


(define some_let_expr '(let ([x 2]) x))
(displayln (list "test..." (check-true (is_complex_and_has_sym? some_let_expr))))


(define complex_val_let_expr '(let ((x (- 2))) 2))
(displayln (list "test..." (check-true (is_complex_and_has_sym? complex_val_let_expr))))

(define complex_body_let_expr '(let ((x 2)) (- 2)))
(displayln (list "test..." (check-true (is_complex_and_has_sym? complex_body_let_expr))))

(define complex_BOTH_let_expr '(let ((x (- 2))) (- 2)))
(displayln (list "test..." (check-true (is_complex_and_has_sym? complex_BOTH_let_expr))))

;TEST ERROR for malformed let expression
(define bad_let_expr '(let (x 2) 2))
(check-fail (λ () (rco_exp bad_let_expr)))

; HASH STUFF
;(hash-eq #hash((1 . 1)) (make-immutable-hash (list (cons 1 1))))
;(and (hash? (make-immutable-hash)) (hash-has-key? (make-hash (list (cons 's 2))) 's))