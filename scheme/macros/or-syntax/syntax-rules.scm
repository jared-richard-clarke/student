;; The 'or' macro as defined by 'syntax-rules'

(define-syntax or
  (syntax-rules ()
    [(_)  #f]
    [(_ x) x]
    [(_ x y z ...)
     (let ([t x])
       (if t 
           t 
           (or y z ...)))]))
