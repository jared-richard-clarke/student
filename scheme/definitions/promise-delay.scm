;; "The syntactic form 'delay' and the procedure 'force' may be used in combination
;;  to implement lazy evaluation. An expression subject to lazy evaluation is not
;;  evaluated until its value is required and, once evaluated, is never reevaluated."
;;
;; — The Scheme Programming Language, chapter 5.7, by R. Kent Dybvig

;; Side Note: These are example implementations. 'promise' and 'delay'
;; are implemented very differently. 'force', for example, must raise 
;; an exception with condition type '&assertion' if its argument is not a promise

(define-syntax delay
  (syntax-rules ()
    [(_ expr) (promise (lambda () expr))]))

(define (promise fn)
  (let ([value #f] [set? #f])
    (lambda ()
      (unless set?
        (let ([x (fn)])
          (unless set?
            (set! value x)
            (set! set? #t))))
      value)))

(define force
  (lambda (promise)
    (promise)))
