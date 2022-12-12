;; The 'or' macro as defined by 'syntax-case' and 'syntax' (#')

(define-syntax or
  (lambda (x)
    (syntax-case x ()
      [(_) #'#f]
      [(_ e) #'e]
      [(_ e1 e2 e3 ...)
       #'(let ([t e1]) 
           (if t 
               t 
               (or e2 e3 ...)))])))
