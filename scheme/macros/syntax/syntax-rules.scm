;; syntax-rules defined in terms of syntax-case.

(define-syntax syntax-rules
  (lambda (x)
    (syntax-case x ()
      [(_ (i ...) ((keyword . pattern) template) ...)
       (syntax (lambda (x)
                 (syntax-case x (i ...)
                   [(_ . pattern) (syntax template)] ...)))])))
