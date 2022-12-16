;; The 'or' macro as defined by 'syntax-case' and 'syntax'

(define-syntax or
  (lambda (stx)
    (syntax-case stx ()
      [(_)   (syntax #f)]
      [(_ x) (syntax x)]
      [(_ x y z ...)
       (syntax (let ([t x]) 
                 (if t 
                     t 
                     (or y z ...))))])))
