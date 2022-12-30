;; (partial function values ...) -> (function values ...) -> value
;; Fixes a number of arguments to a function, producing another function with a smaller arity.
;; Applies function to combined arguments.
;; (define add1 (partial + 1)) -> (add1 10 10) -> 21

(define (partial fn . x)
  (lambda y
    (apply fn (append x y))))
