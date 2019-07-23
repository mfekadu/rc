
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
     ; below check not needed because match read alone
     ; (check-true (or (> (length args) 0) (eq? op 'read)))

     (print (list "args:" args))
     ;(for 
     
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
    [(? list? l)

     (define tmp_name (gensym "tmp"))

     (define alist (hash tmp_name (rco_exp l)))

     (list tmp_name alist)
     ]
    [_ e]))



; TEST CASES
; ATOMS should stay simple
(check-equal? (rco_exp 2) 2)
(check-equal? (rco_exp '+) '+)

; BAD exprs
(check-equal? (rco_exp (list 2)) "panic!")
(check-equal? (rco_exp '(x)) "panic!")
(check-equal? (rco_exp (list '+)) "panic!")

; SIMPLE exprs SHOULD STAY SIMPLE
(check-equal? (rco_exp (list '+ 2 2)) '(+ 2 2))
(check-equal? (rco_exp '(let (x 2) x)) '(let (x 2) x))