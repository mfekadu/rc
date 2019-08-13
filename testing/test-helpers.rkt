#!/usr/local/bin/racket
#lang racket
(require rackunit)
(provide check-fail check-fail-with-name)

; https://docs.racket-lang.org/rackunit/api.html#%28def._%28%28lib._rackunit%2Fmain..rkt%29._check-exn%29%29
; given a thunk/lambda/procedure/funciton... make sure it fails
(define (check-fail thunk) (check-exn exn:fail? thunk))

; test check-fail
(check-true (void? (check-fail (λ () (error)))))


; setup namespace for eval
; https://stackoverflow.com/a/37246112
(current-namespace (make-base-namespace))

; given the symbol name of a function
; and the given_arg to pass into the function
; make sure it fails and the error message contains the fun_name
(define (check-fail-with-name fun_name fun_proc given_arg)
  (cond [(symbol? fun_name)
         (define regex_str (string-append (symbol->string fun_name) ".*"))
         (check-exn (regexp regex_str) (λ () (fun_proc given_arg)))]
        [else
         (error 'check-fail-with-name "bad fun_name: ~v" fun_name)]))
; test check-fail-with-name
(check-true (void? (check-fail-with-name 'error error 'error)))

; test bad args
(check-fail (λ () (check-fail-with-name 32 42)))

; test it catches diff name failure
(define fn_err_with_diff_name (λ (bar) (error 'foo "wow ~v" bar)))
(check-fail (λ () (fn_err_with_diff_name 'foo))) ; make sure it err by itself
(define thunk (λ () (check-fail-with-name 'fn_err_with_diff_name fn_err_with_diff_name "given_arg")))
(check-true (void? (check-fail thunk))) ; test case

; test it catches allows same name failure
(define fn_err_same_name (λ (bar) (error 'fn_err_same_name "wow ~v" bar)))
(check-fail (λ () (fn_err_same_name 'foo))) ; make sure it err by itself
(define res (check-fail-with-name 'fn_err_same_name fn_err_same_name 'foo))
(check-true (void? res)) ; test case
