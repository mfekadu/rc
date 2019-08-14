#!/usr/local/bin/racket
#lang racket
(provide rco-prog)

; tests
(provide rco)
(provide rco-arg)

; top level rco function. Takes an R1 program and returns an R1 program without complex expressions.
(define (rco-prog prog)
  (match prog
    [`(program ,locals (,label ,(? list? e)))
     ; Strictly follow the R1 grammar for now because '(start (+ 2 2)) is not an expr
     ; Feel free to refactor for R2.
     (error 'rco-prog "Malformed expr in program: ~s" prog)]
    [`(program ,locals ,e)
     `(program ,locals ,(rco e))]
    [_ (error 'rco-prog "Malformed program: ~s" prog)]))

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