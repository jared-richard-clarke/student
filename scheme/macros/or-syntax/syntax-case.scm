;; The 'or' macro as defined by 'syntax-case' and 'syntax'

(define-syntax or
  (lambda (x)
    (syntax-case x ()
      [(_) (syntax #f)]
      [(_ e) (syntax e)]
      [(_ e1 e2 e3 ...)
       (syntax (let ([t e1]) 
                 (if t 
                     t 
                     (or e2 e3 ...))))])))
