#!/usr/bin/racket
#lang racket
(require rackunit)

(define (make-let var val body)
  (list 'let (list [list var val]) body))

(define (create-all-bindings body binding-list)
  42)

; Given an expr in R1, return an expr in R1 without any complex subexpressions
; Calls rco-exp on the input expression, which should recursively call rco-arg or rco-exp
; rco gets a simple expression (one of (read), (- tmp), (+ tmp1 tmp2))) and an association
; list with all the bindings that need to be created. The bindings MUST be ordered according
; to scope (e.g., if tmp2 is bound to (- tmp1), then tmp2 must come BEFORE tmp1 in the alist).
; rco then iterates through the alist and creates nested bindings, where the simple-expr is 
; treated as the body of the binding. On each iteration, the body is updated to become the
; newly created let-expression.
(define (rco exprs) ; returns expr
  (define-values [simple-expr alist] (rco-exp exprs))
  (define not-empty (lambda (lst) (not (empty? lst))))
  (define bindings-to-make (filter not-empty alist))
  (create-all-bindings simple-expr bindings-to-make))

; Given an expr in R1, return a simple expr in R1 and an association list
(define (rco-exp exprs)
  (match exprs
    [(or (? symbol?) (? integer?) '(read)) (values exprs '())]
    ; defer to rco-arg for let case
    [(list 'let (list [list var val]) body) (rco-arg exprs)]
    ; iterate through expression list and call rco-arg on each argument
    [(list op args ...) 
     (define-values [syms bindings]
       (for/fold  ([syms '()]
                   [bindings '()])
                  ([e exprs]) 
         (define-values [symbol alist] (rco-arg e))
         (values (append syms (list symbol)) (append alist bindings))))
     (values syms bindings)]))

; Given an expr in R1, return a temp symbol name and an alist mapping from the symbol name to an expr in R1
; returns expr, alist
(define (rco-arg exprs)
  (match exprs
    ; handle simple base cases
    [(or (? symbol?) (? integer?) '(read)) (values exprs '())]
    ; TODO let case should bind var to val in an alist and evaluate the body somehow 
    [(list 'let (list [list var val]) body) (error "rco-arg let case")]
    [(list op args ...)
     (define tmp-name (gensym 'tmp))
     ; recursively call rco-exp on this expression 
     (define-values [syms alist] (rco-exp exprs))
     (values tmp-name
             ; add the newest binding to the front of the list
             ; TODO is this the correct ordering?
             (append (list (list tmp-name syms)) alist))]))

; TEST HELPERS
(define make-list-from-vals (λ (a b) (list a b)))

(define (verify-rco-arg-output-is-empty given)
  (check-match 
    (call-with-values (λ () (rco-arg given)) make-list-from-vals) 
    (list given (? empty?))))

(define (verify-rco-arg-output given expect)
  (check-match 
    (call-with-values (λ () (rco-arg given)) make-list-from-vals) 
    (list (? symbol? s) (list (? symbol? s) expect))))

(rco-exp '(+ 2 2))
(rco-exp '(+ 2 (- 3)))
(rco-exp '(+ (- 2) 3))
(rco-exp '(+ (- 2) (- 3)))
(rco-exp '(+ (- (- 2)) 3))
; TEST CASES
; ATOMS should stay simple rco-exp
;(check-equal? (rco-exp 2) 2)
;(check-equal? (rco-exp '+) '+)
;; ATOMS should stay simple rco-arg
;(verify-rco-arg-output-is-empty 2)
;(verify-rco-arg-output-is-empty '+)
;
;
;; OPERATIONS should get simplied by rco-arg
;(verify-rco-arg-output '(+ 2 2) '(+ 2 2))
;
;; SIMPLE OPERATIONS should stay simple when called by rco-exp
;(check-equal? (rco-exp '(+ 2 2)) '(+ 2 2))
;
;(displayln "yes")
;(rco-exp '(+ 2 (- (+ 3 4))))
;(rco-arg '(let ([x 1]) x))
;
;; BAD exprs rco-exp
;(check-equal? (rco-exp (list 2)) "panic!")
;(check-equal? (rco-exp '(x)) "panic!")
;(check-equal? (rco-exp (list '+)) "panic!")
;(check-equal? (rco-exp #t) "panic!")
;; BAD exprs rco-arg
;(check-equal? (rco-arg #t) "panic!")
;
;; SIMPLE exprs SHOULD STAY SIMPLE
;(check-equal? (rco-exp (list '+ 2 2)) '(+ 2 2))
;(check-equal? (rco-exp '(let ([x 2]) x)) '(let ([x 2]) x))
;(check-equal? (rco-exp (list 'read)) '(read))
;

 

(displayln "tests finished")
