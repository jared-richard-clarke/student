;; (λ formals expression ...)
;; Defines lowercase λ as an alternative symbol to lambda.
;; (λ (x y) (+ x y)) -> (lambda (x y) (+ x y))

(define-syntax λ
  (lambda (stx)
    (syntax-case stx ()
      [(_ x y z ...)
       (syntax (lambda x y z ...))])))

;; Replicates the "defun" syntax from Common Lisp
;;
;; (defun add (x y) (+ x y)) ->
;; (define add (lambda (x y) (+ x y)))

(define-syntax defun
  (lambda (stx)
    (syntax-case stx ()
      [(_ x y z w ...)
       (syntax (define x (lambda y z w ...)))])))
