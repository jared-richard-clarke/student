;; (pipe any function ...) -> any
;; Transforms a value by passing it through a series of single-argument functions. 
;; The first function can take any number of arguments. All proceeding functions must take one.
;; Functions are applied left to right.
;; (pipe "hElLo" lowercase capitalize) -> "Hello"

(define (pipe arg . functions)
  (fold-left (lambda (value function)
               (function value))
             arg
             functions))

;; (compose function ...) -> (function any) -> any
;; Composes a series of functions into a single function expression.
;; The first function can take any number of arguments. All proceeding functions must take one.
;; Functions are applied right to left.
;; (map (compose - abs) '(1 -2 7 -11)) -> '(-1 -2 -7 -11)

(define (compose . functions)
  (lambda (arg)
    (fold-right (lambda (function value)
                  (function value))
                arg
                functions)))
