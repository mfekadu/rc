#!/usr/bin/racket
#lang racket
(require rackunit)

(define (make-let var val body)
  (list 'let (list [list var val]) body))

(define (create-all-bindings body binding-list)
  (for/fold ([final-expr body])
            ([binding binding-list])
    (make-let (first binding) (first (rest binding)) final-expr)))

; Given an expr in R1, return an expr in R1 without any complex subexpressions
; Calls rco-exp on the input expression, which should recursively call rco-arg or rco-exp
; rco gets a simple expression (one of (read), (- tmp), (+ tmp1 tmp2))) and an association
; list with all the bindings that need to be created. The bindings MUST be ordered according
; to scope (e.g., if tmp2 is bound to (- tmp1), then tmp2 must come BEFORE tmp1 in the alist).
; rco then iterates through the alist and creates nested bindings, where the simple-expr is 
; treated as the body of the binding. On each iteration, the body is updated to become the
; newly created let-expression.
(define (rco exprs)
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
         (values (append syms (list symbol))
                 (append alist bindings))))
     (values syms bindings)]))

; Given an expr in R1, return a temp symbol name and an alist mapping from the symbol name to an expr in R1
; returns expr, alist
(define (rco-arg exprs)
  (match exprs
    ; handle simple base cases
    [(or (? symbol?) (? integer?) '(read)) (values exprs '())]
    [(list 'let (list [list var val]) body)
     ; call rco-arg on body since the body must be replaced with a temp if it's complex at all
     ; e.g. if body is (+ x 1) , we want to replace that with a temp and a new binding
     (define-values [body-sym body-alist] (rco-arg body))

     ; val can stay complex to an extent (if it's (+ 2 2) that's totally fine, but not if it has
     ; nested subexpressions)
     (define-values [val-syms val-alist] (rco-exp val))

     ; body-alist should be bound before the val-alist since bindings in the body-alist might depend
     ; on bindings defined in the val-alist
     ; If you think of this program sequentially, bindings defined by the value must occur before anything
     ; is bound in the body
     ; Similarly, anything in val-syms will depend on the bindings in the val-alist, and bindings in body-alist
     ; will depend on the Var
     (define return-alist (append body-alist (list (list var val-syms)) val-alist))

     ; return the body-sym since that is the expression that is actually evaluated in a let-expression
     (values body-sym return-alist)]
    [(list op args ...)
     (define tmp-name (gensym 'tmp))
     ; recursively call rco-exp on this expression 
     (define-values [syms alist] (rco-exp exprs))
     (values tmp-name
             ; add the newest binding to the front of the list
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

(rco '(+ 2 2))
(rco '(+ 2 (- 3)))
(rco '(+ (- 2) 3))
(rco '(+ (- 2) (- 3)))
(rco '(+ (- (- 2)) 3))
(rco '(+ (- (- 2)) (+ 3 (- 4))))
(rco '(let ([x 1]) x))
(rco '(+ 4 (let ([x 1]) x)))
(rco '(+ (let ([x (- (- 1))]) (+ x (- 2))) 40))
(rco '(let ([a 42]) (let ([b a]) b)))
(rco '(let ([y (let ([x 20]) x)]) (+ y 1)))

; this was the test case we were failing before in rust
(rco '(let ([y (let ([x1 20]) (+ x1 (let ([x2 22]) x2)))]) y))


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
