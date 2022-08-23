;; (λ formals expression ...)
;; Defines lowercase λ as an alternative symbol to lambda.
;; (λ (x y) (+ x y)) -> (lambda (x y) (+ x y))

(define-syntax λ
  (syntax-rules ()
    [(_ formals x y ...)
     (lambda formals x y ...)]))

;; Defines lowercase florin (ƒ) as an alternative symbol to lambda.

(define-syntax ƒ
  (syntax-rules ()
    [(_ formals x y ...)
     (lambda formals x y ...)]))

;; ᓴᓐᓇ translates to Sedna. She is the Inuit goddess of the sea and marine mammals.
;; A function/procedure in Scheme is a an identifier, a list of formals, and a body of one or more expressions.
;; The identifier for a Scheme function is "lambda" in reference to the lambda calculus.
;; This symbol could have been anything.

(define-syntax ᓴᓐᓇ
  (syntax-rules ()
    [(_ formals x y ...)
     (lambda formals x y ...)]))
