;; syntax-rules defined in terms of syntax-case.

(define-syntax syntax-rules
  (lambda (stx)
    (syntax-case stx ()
      [(_ (i ...) ((keyword . pattern) template) ...)
       (syntax (lambda (stx)
                 (syntax-case stx (i ...)
                   [(_ . pattern) (syntax template)] ...)))])))
