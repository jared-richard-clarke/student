;; "The syntactic form 'delay' and the procedure 'force' may be used in combination
;;  to implement lazy evaluation. An expression subject to lazy evaluation is not
;;  evaluated until its value is required and, once evaluated, is never reevaluated."
;;
;; — The Scheme Programming Language, chapter 5.7, by R. Kent Dybvig

(define-syntax delay
  (syntax-rules ()
    [(_ expression) 
     (promise (lambda () expression))]))

(define promise
  (lambda (thunk)
    (unless (procedure? thunk)
      (assertion-violation 'promise "argument is not a procedure" thunk))
    (let ([value #f] [set? #f])
      (lambda ()
        (case set?
          [(single) value]
          [(multiple) (apply values value)]
          [else
           (call-with-values
             thunk
             (case-lambda
               [(x)
                (case set?
                  [(single) value]
                  [(multiple) (apply values value)]
                  [(#f) (set! value x)
                        (set! set? 'single)
                        x])]
               [xs
                (case set?
                  [(single) value]
                  [(multiple) (apply values value)]
                  [(#f) (set! value xs)
                        (set! set? 'multiple)
                        (apply values xs)])]))])))))
(define force
  (lambda (promise)
    (unless (procedure? promise)
      (assertion-violation 'force "argument is not a procedure" promise))
    (promise)))
