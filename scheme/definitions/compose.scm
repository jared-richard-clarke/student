;; (pipe value function ...) -> value
;; Transforms a value by passing it through a series of single-argument functions. 
;; The first function can take any number of arguments. All proceeding functions must take one.
;; (pipe "dog" string-plural string-upcase) -> "DOGS"

(define (pipe arg . functions)
  (foldl (lambda (function value)
           (function value))
         arg
         functions))

;; (compose function ...) -> (function value) -> value
;; Composes a series of functions into a single function expression.
;; The first function can take any number of arguments. All proceeding functions must take one.
;; (define edit (compose string-plural string-upcase)) -> (edit "dog") -> "DOGS"

(define (compose . functions)
  (lambda (arg)
    (foldl (lambda (function value)
             (function value))
           arg
           functions)))
