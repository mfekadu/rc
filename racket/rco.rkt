
#lang racket
(require rackunit)

(define (rco_exp e)
  (match e
    [(? number? n) n]
    [(? symbol? s) s]
    ; match the read alone before next list match
    [(list 'read) e]
    [(list other) "panic!"]
    ; match let binding
    [(list 'let bind body)

     ;(check-true (pair? bind)) ; pls be ok

     (match bind
       [(list var val) (println (list "var" var "res" (rco_exp val)))])

     (define res1 (rco_arg body))
     (println (list "res1" res1))
     ;(define res2 (rco_arg n2))
     
     (list 'let bind body)
     ]
    ; match operation and rest of args
    [(list op args ...)

     (check-true (<= (length args) 2))

     (println (list "args:" args))
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
  (match e
    [(? number? n) n]
    [(? symbol? s) s]
    [(? list? l)

     (define tmp_name (gensym "tmp"))

     (define alist (hash tmp_name (rco_exp l)))

     (list tmp_name alist)
     ]
    [_ "panic!"]))



; TEST CASES
; ATOMS should stay simple
(check-equal? (rco_exp 2) 2)
(check-equal? (rco_exp '+) '+)
; ATOMS should stay simple
(check-equal? (rco_arg 2) 2)
(check-equal? (rco_arg '+) '+)

; BAD exprs
(check-equal? (rco_exp (list 2)) "panic!")
(check-equal? (rco_exp '(x)) "panic!")
(check-equal? (rco_exp (list '+)) "panic!")
(check-equal? (rco_exp #t) "panic!")
; BAD exprs
(check-equal? (rco_arg #t) "panic!")

; SIMPLE exprs SHOULD STAY SIMPLE
(check-equal? (rco_exp (list '+ 2 2)) '(+ 2 2))
(check-equal? (rco_exp '(let (x 2) x)) '(let (x 2) x))
(check-equal? (rco_exp (list 'read)) '(read))
;; SIMPLE exprs SHOULD STAY SIMPLE
;(check-true (match (rco_arg (list '+ 2 2))
;                  [(make-immutable-hash (list (cons 'tmp31467 (list '+ 2 2)))) ]))
;(check-equal? (rco_arg '(let (x 2) x)) '(let (x 2) x))
;(check-equal? (rco_arg (list 'read)) '(read))



;(hash-eq #hash((1 . 1)) (make-immutable-hash (list (cons 1 1))))