;; syntax-rules defined in terms of syntax-case.
;;
;; "Transformers normally destructure their input with 'syntax-case' and rebuild their output with 'syntax'."
;; — The Scheme Programming Language, section 8.3, R. Kent Dybvig

(define-syntax syntax-rules
  (lambda (stx)
    (syntax-case stx ()
      [(_ (i ...) ((keyword . pattern) template) ...)
       (syntax (lambda (stx)
                 (syntax-case stx (i ...)
                   [(_ . pattern) (syntax template)] ...)))])))
