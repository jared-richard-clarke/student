;; (λ formals expression ...)
;; Defines alternative symbol for lambda.
;; (λ (x y) (+ x y)) -> (lambda (x y) (+ x y))

(define-syntax λ
  (syntax-rules ()
    ((_ formals expression ...)
     (lambda formals expression ...))))
