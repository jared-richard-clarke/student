;; (pipe value function ...) -> value
;; Transforms a value by passing it through a series of single-argument functions. 
;; (pipe "dog" string-plural string-upcase) -> "DOGS"

(define (pipe arg . functions)
  (foldl (lambda (function value)
           (function value))
         arg
         functions))

;; (comp function ...) -> (function value) -> value
;; Composes a series of single-argument functions into a single function expression.
;; (define edit (comp string-plural string-upcase)) -> (edit "dog") -> "DOGS"

(define (comp . functions)
  (lambda (arg)
    (foldl (lambda (function value)
             (function value))
           arg
           functions)))
