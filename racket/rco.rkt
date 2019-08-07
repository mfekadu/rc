#!/usr/bin/racket
#lang racket
(require rackunit)
(provide rco-prog)

; top level rco function. Takes an R1 program and returns an R1 program without complex expressions.
(define (rco-prog prog)
  (match prog
    [`(program ,locals (,label ,expr))
      `(program ,locals (,label ,(rco expr)))]
    [_ (error "malformed input program to rco-prog")]))

(define (make-let var val body) (list 'let (list [list var val]) body))
  
(define (create-all-bindings body binding-list)
  (for/fold ([final-expr body])
            ([binding binding-list])
    (make-let (first binding) (first (rest binding)) final-expr)))

; Given an expr in R1, return an expr in R1 without any complex subexpressions
; Calls rco-exp on the input expression, which should recursively call rco-arg or rco-exp
; rco-exp returns a simple expression (one of (read), (- tmp), (+ tmp1 tmp2))) and an association
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

; TEST CASES
(check-equal? (rco '(+ 2 2)) '(+ 2 2))

(check-match (rco '(+ 2 (- 3))) 
             `(let ([,(? symbol? s) (- 3)]) (+ 2 ,(? symbol? s))))

(check-match  (rco '(+ (- 2) 3))
             `(let ([,(? symbol? s) (- 2)]) (+ ,(? symbol? s) 3)))

(check-match (rco '(+ (- 2) (- 3)))
             `(let ([,(? symbol? s1) (- 2)]) (let ([,(? symbol? s2) (- 3)]) (+ ,(? symbol? s1) ,(? symbol? s2)))))

(check-match (rco '(+ (- (- 2)) 3))
             `(let ([,(? symbol? s1) (- 2)]) (let ([,(? symbol? s2) (- ,s1)]) (+ ,s2 3))))

(check-match (rco '(+ (- (- 2)) (+ 3 (- 4))))
             `(let ([,(? symbol? neg2) (- 2)]) 
                (let ([,(? symbol? negneg2) (- ,neg2)]) 
                  (let ([,(? symbol? neg4) (- 4)])
                    (let ([,(? symbol? plus3neg4) (+ 3 ,neg4)])
                      (+ ,negneg2 ,plus3neg4))))))

; setup namespace for eval
; https://stackoverflow.com/a/37246112
(current-namespace (make-base-namespace))

; test does exactly what you think it does
(define (verify-rco-evals-correctly given)
  (check-equal? (eval (rco given)) (eval given)))

;
(define given '(let ([x 1]) x))
(verify-rco-evals-correctly given)
(check-equal? (rco given) given) 

;
(define given2 '(+ 4 (let ([x 1]) x)))
(verify-rco-evals-correctly given2)
(check-equal? (rco given2) 
              '(let ([x 1]) (+ 4 x)))

;
(define given3 '(+ (let ([x (- (- 1))]) (+ x (- 2))) 40))
(verify-rco-evals-correctly given3)
(check-match (rco given3)
             `(let ([,(? symbol? neg1) (- 1)]) 
                (let ([x (- ,neg1)]) 
                  (let [[,(? symbol? neg2) (- 2)]] 
                    (let [[,(? symbol? plusxneg2) (+ x ,neg2)]] (+ ,plusxneg2 40))))))


(define given4 '(let ([a 42]) (let ([b a]) b)))
(verify-rco-evals-correctly given4)
(check-equal? (rco given4) given4)

; rco output is correct but not optimal - extra temp is introduced
; this test is meant to fail
(define given5 '(let ([y (let ([x 20]) x)]) (+ y 1)))
(verify-rco-evals-correctly given5)
(check-match (rco given5)
             `(let ([x 20]) 
                (let ([y x])
                  (+ y 1))))

; same as above
(define given6 '(let ([y (let ([x1 20]) (+ x1 (let ([x2 22]) x2)))]) y))
(verify-rco-evals-correctly given6)
(check-match (rco given6)
             `(let ([x1 20])
                (let ([x2 22])
                  (let ([y (+ x1 x2)])
                    y))))


; testing rco-prog
(define given3-prog `(program () (start ,given3)))
(check-match (rco-prog given3-prog)
             `(program () (start (let ([,(? symbol? neg1) (- 1)]) 
                                    (let ([x (- ,neg1)]) 
                                      (let [[,(? symbol? neg2) (- 2)]] 
                                        (let [[,(? symbol? plusxneg2) (+ x ,neg2)]] (+ ,plusxneg2 40))))))))
