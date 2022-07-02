;; (λ formals expression ...)
;; Defines alternative symbol for lambda.
;; (λ (x y) (+ x y)) -> (lambda (x y) (+ x y))

(define-syntax λ
  (syntax-rules ()
    ((_ formals expression ...)
     (lambda formals expression ...))))

;; ᓴᓐᓇ translates to Sedna. She is the Inuit goddess of the sea and marine mammals.
;; A function/procedure in Scheme is a an identifier, a list of formals, and a body of one or more expressions.
;; The identifier for functions in Scheme is "lambda" in reference to the lambda calculus; however, the 
;; keyword could have been any symbol.

(define-syntax ᓴᓐᓇ
  (syntax-rules ()
    [(_ formals expression ...)
     (lambda formals expression ...)]))
