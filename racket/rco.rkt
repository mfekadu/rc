
#lang racket
(require rackunit)

(define (rco_exp e)
  (match e
    [(? number? n) n]
    [(? symbol? s) s]
    ;[(list (? symbol? s)) (list s)]
    [(list '+ n1 n2)
     
     (define res1 (rco_arg n1))
     (define res2 (rco_arg n2))

     ; do magic on res1 res2

     (list '+ res1 res2)
     
      ]
    [(list 'let bind body)

     ;(check-true? (pair? bind)) ; pls be ok

     (match bind
       [(list var val) (println (list "var" var "res" (rco_exp val)))])

     (define res1 (rco_arg body))
     (println (list "res1" res1))
     ;(define res2 (rco_arg n2))
     
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
     ])
  2)



; test cases
(rco_exp 2)
;(rco_exp (list 2))
;(rco_exp '+)
;(rco_exp '(x))
;(rco_exp (list '+))
;(rco_exp (list '+ 2 2)) ; out = '(+ 2 2)

(rco_exp '(let (x 2) x)) ; out = '(let (x 2) x)