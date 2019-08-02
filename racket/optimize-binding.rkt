#!/usr/bin/racket
#lang racket
(require rackunit)


; given a list / non-list `lst` and a value `v`
; return #t if lst is a list? and if v in lst else #f
; OR return #t if lst not a list? and if v equal? lst else #f
(define (contains-or-equal? v lst)
  (cond [(list? lst)
         ; `member` returns a list if v found in lst; else #f
         (list? (member v lst))]
        [else (equal? lst v)]))
; ******************************
; test contains-or-equal?
; ******************************
(check-true (contains-or-equal? 'x 'x))
(check-true (contains-or-equal? 'x '(x)))
(check-true (contains-or-equal? 'x '(1 x)))
(check-true (contains-or-equal? 'x '(1 x 2)))
(check-true (contains-or-equal? 'x '(x 2)))
(check-false (contains-or-equal? 'x '()))
(check-false (contains-or-equal? 'x 'y))


; given some expr,
; return #t if is-let else #f
(define (is-let? expr)
  (match expr
    [`(let [[,(? symbol? var) ,val]] ,body) #t]
    [_ #f]))
; ******************************
; test is-let?
; ******************************
(check-true (is-let? '(let [[x 1]] x)))
(check-false (is-let? '(let [x 1] x)))
(check-false (is-let? 'x))
(check-false (is-let? '(let bind body)))
(check-false (is-let? 42))
(check-false (is-let? #t))
(check-false (is-let? #f))
(check-false (is-let? '(let [[1 x]] 42)));

; given a let-expr and a hashmap of unused-vars
; return and optimized version such that
; non-optimized is ......(let [[x1 20]] (let [[tmp (+ x1 22)]] (let [[y tmp]] y)))
; which becomes .........(let [[x1 20]] (let [[y (+ x1 22)]] y))
; OR non-optimized is ...(let [[x1 20]] (let [[tmp (+ x1 22)]] tmp))
; which becomes .........(let [[x1 20]] (+ x1 22))
; NOTE: only use `optimize-binding` after `uniquify`
;       because otherwise weird stuff could happen when we hash-set?
; NOTE; only use `optimize-binding` after remove-complex-opera
;       becuase it assumes that the ,val of each binding is already simple expr
;       also because it assumes the ,val is not a binding as well (i.e. simple expr)
(define (optimize-binding let-expr unused-vars)
  (match let-expr
    [`(let [[,(? symbol? v) ,v]] ,body)
     ; if let-exp looks like '(let [[x x]] body)
     ; ideally uniquify should dissallow this
     (error 'optimize-binding "~v: bad binding of variable to itself?" v)]
    [`(let [[,(? symbol? var) ,(? symbol? val)]] ,body)
     #:when (and (is-let? body) (hash-has-key? unused-vars val))
     ; if ,body is let and the unused variable is now being used
     ; NOTE: var must be unique / new / unused now here because we ran uniquify already
     (define new-hash (hash-set unused-vars var val)) ; add this new ,var to unused-vars
     (displayln (list 'new-hash new-hash))
     (define new-hash2 (hash-remove new-hash val)) ; remove ,val from unused-vars because it just got used
     (displayln (list 'new-hash2 new-hash2))
     (optimize-binding body new-hash2)]
    [`(let [[,(? symbol? var) ,(? symbol? val)]] ,body)
     #:when (not (hash-has-key? unused-vars val))
     ; if in any way val was not defined
     ; or optimize-binding accidentally removed a would-be-used symbol
     (error 'optimize-binding "~v: undefined; cannot reference a variable before its definition" val)]
    [`(let [[,(? symbol? var) ,(? list? val)]] ,body)
     #:when (and (is-let? body))
     ; if ,body is let and the ,val is operation
     ; then check for all prev unused variables inside operation
     (define is-nope? (λ (v) (not (equal? v "nope"))))
     (define get-key-if-hash-has-key (λ (v) (cond [(hash-has-key? unused-vars v) v] [else "nope"])))
     (define hash-keys-or-nope (map get-key-if-hash-has-key val))
     (displayln `(hash-keys-or-nope ,hash-keys-or-nope))
     (define prev-used-hash-keys (filter is-nope? hash-keys-or-nope))
     (displayln (list 'prev-used-hash-keys prev-used-hash-keys))
     (define remove (λ (k h) (hash-remove h k)))
     (define new-hash (foldl remove unused-vars prev-used-hash-keys))
     (displayln (list 'new-hash new-hash))
     (define new-hash2 (hash-set new-hash var val)) ; add this new ,var to unused-vars
     (displayln (list 'new-hash2 new-hash2))
     (optimize-binding body new-hash2)]
    [`(let [[,(? symbol? var) ,val]] ,body)
     #:when (is-let? body)
     ; if ,body is a let and var is a brand new unused var
     ; NOTE: 
     (define new-hash (hash-set unused-vars var val))
     (displayln (list 'new-hash new-hash))
     (optimize-binding body new-hash)]
    [`(let [[,(? symbol? var) ,val]] ,body)
     #:when (and (hash-has-key? unused-vars var) (contains-or-equal? var val))
     ; if ,body is NOT let and the unused var is now being used
     (define new-hash (hash-remove unused-vars var))
     (displayln new-hash)
     let-expr]
    [`(let [[,(? symbol? var) ,val]] ,body)
     ; if ,body is NOT let
     (define new-hash (hash-set unused-vars var val))
     (displayln new-hash)
     let-expr]
    ; anything else should stay the same
    [_ let-expr]))

; goal is to try to make this all work with just optimize-binding
; such that the proper let gets returned by optimize-binding
; becuase it can create the let on the way back up the recursive stack
; if that goal fails
; then optimize-binding needs some sort of a helper to make the nested lets
; because optimize-binding will try to return some sort of association list
; of bindings
; which means no more hash-table because hash-table does not preserve order
; the recursive call stack does however preserve order
; so let's try that first
; but good thing to know there is a plan b. 


; test anything else should stay the same
(check-equal? (optimize-binding '(+ 2 2) (hash)) '(+ 2 2))

(displayln '(let [[x1 20]] (let [[tmp (+ x1 22)]] (let [[y tmp]] y))))
(check-equal? (optimize-binding '(let [[x1 20]] (let [[tmp (+ x1 22)]] (let [[y tmp]] y)))
                  (hash))
              '(let [[x1 20]] (let [[y (+ x1 22)]] y)))

(displayln '(let [[x1 20]] (let [[tmp (+ x1 22)]] tmp)))
(check-equal? (optimize-binding '(let [[x1 20]] (let [[tmp (+ x1 22)]] tmp))
                  (hash))
              '(let [[x1 20]] (+ x1 22)))