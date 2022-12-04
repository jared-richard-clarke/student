;; (pipe any function ...) -> any
;; Transforms a value by passing it through a series of single-argument functions. 
;; The first function can take any number of arguments. All proceeding functions must take one.
;; Functions are applied left to right.
;; (pipe 1 add1 div2) -> 1

(define (pipe arg . functions)
  (fold-left (lambda (value function)
               (function value))
             arg
             functions))

;; (compose function ...) -> (function any) -> any
;; Composes a series of functions into a single function expression.
;; The first function can take any number of arguments. All proceeding functions must take one.
;; Functions are applied left to right.
;; (define add-div (compose add1 div2)) -> (add-div 1) -> 1

(define (compose . functions)
  (lambda (arg)
    (fold-left (lambda (function value)
                 (function value))
               arg
               functions)))

;; (>> functions ...) -> (function any) -> any
;; Alias for compose. Named after the composition operator in #F.

(define (>> . functions)
  (apply compose functions))

;; (<< functions ...) -> (function any) ->  any
;; Applies functions in reverse order. Named after the reverse composition operator in #F.

(define (<< . functions)
  (let ([functions (reverse functions)])
    (apply compose functions)))
