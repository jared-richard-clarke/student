;; (assert predicate expression expression) -> current-output-port
;; If the left-hand-expression does not satisfy the predicate comparing it to the right-hand-expression,
;; "assert" prints the failed test to the current-output port.
;; (assert equal? (+ 1 6) (- 1 6)) ->
;; "Test failed:
;; lhs: (+ 1 6) -> 7, rhs: (- 1 6) -> -5"

(define-syntax assert
  (syntax-rules ()
    [(_ compare x y)
     (let ([computed-x x]
           [computed-y y])
       (unless (compare computed-x computed-y)
         (printf "Test failed:\nlhs: ~a -> ~a, rhs: ~a -> ~a\n"
                 (quote x)
                 computed-x
                 (quote y)
                 computed-y)))]))

;; Version of "assert" compatible with "rnrs"

(define-syntax assert
  (syntax-rules ()
    [(_ compare x y)
     (let ([computed-x x]
           [computed-y y])
       (unless (compare computed-x computed-y)
         (begin (display "Test failed:")
                (newline)
                (display "lhs: ") (write (quote x)) (display " -> ") (write computed-x) (display ", ")
                (display "rhs: ") (write (quote y)) (display " -> ") (write computed-y)
                (newline))))]))
