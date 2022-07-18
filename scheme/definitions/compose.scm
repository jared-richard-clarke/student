;; (pipe value function ...) -> any
;; Transforms a value by passing it through a series of single-argument functions. 
;; The first function can take any number of arguments. All proceeding functions must take one.
;; (pipe "dog" string-plural string-upcase) -> "DOGS"

(define (pipe arg . functions)
  (foldl (lambda (function value)
           (function value))
         arg
         functions))

;; (compose function ...) -> (function any) -> any
;; Composes a series of functions into a single function expression.
;; The first function can take any number of arguments. All proceeding functions must take one.
;; (define edit (compose string-plural string-upcase)) -> (edit "dog") -> "DOGS"

(define (compose . functions)
  (lambda (arg)
    (foldl (lambda (function value)
             (function value))
           arg
           functions)))

;; (>> functions ...) -> (function any) -> any
;; Alias for compose. Named after the composition operator in OCaml.

(define (>> . functions)
  (apply compose functions))

;; (<< functions ...) -> (function any) ->  any
;; Applies functions in reverse order. Named after the reverse composition operator in OCaml.

(define (<< . functions)
  (let ([reversion (reverse functions)])
    (apply compose reversion)))
