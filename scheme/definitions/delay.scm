;; > "The syntactic form delay and the procedure force may be used
;; > in combination to implement lazy evaluation. An expression subject
;; > to lazy evaluation is not evaluated until its value is required and,
;; > once evaluated, is never reevaluated."
;; >
;; > [The Scheme Programming Language](https://www.scheme.com/tspl4/control.html#./control:h0)
;; > by R. Kent Dybvig

(define-syntax delay
  (syntax-rules ()
    [(_ expr) (make-promise (lambda () expr))]))

(define (make-promise p)
  (let([cache #f]
       [set? #f])
    (lambda ()
      (unless set?
        (set! cache (p))
        (set! set? #t))
      cache)))

(define (force promise)
  (promise))
