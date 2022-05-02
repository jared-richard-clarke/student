;; (pipe value function ...) -> value
;; Transforms a value by passing it through a series of single-argument functions. 
;; (pipe "dog" string-plural string-upcase) -> "DOGS"

(define (pipe arg . functions)
  (foldl (lambda (function value)
           (function value))
         arg
         functions))

;; (compose-pipe function ...) -> (function value) -> value
;; Composes a series of single-argument functions into a single function expression.
;; (define edit (compose-pipe string-plural string-upcase)) -> (edit "dog") -> "DOGS"

(define (compose-pipe . functions)
  (lambda (arg)
    (foldl (lambda (function value)
             (function value))
           arg
           functions)))
