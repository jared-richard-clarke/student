;; (complement function) -> (function values ...) -> boolean
;; Inputs a function and outputs a new function that has the same arguments
;; and effects, if any, as the original function but returns the opposite boolean value.
;; (define not-number? (complement number?)) -> (not-number? "cat") -> #t

(define (complement fn)
  (lambda args
    (not (apply fn args))))
