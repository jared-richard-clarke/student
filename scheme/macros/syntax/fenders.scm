;; This syntax definition of 'let' uses the fender 'ids?'
;; to ensure each binding uses a proper identifier.

(define-syntax let
  (lambda (x)
    (define ids?
      (lambda (lst)
        (or (null? lst)
            (and (identifier? (car lst))
                 (ids? (cdr lst))))))
    (syntax-case x ()
      [(_ ((i e) ...) b1 b2 ...)
       (ids? #'(i ...))
       #'((lambda (i ...) b1 b2 ...) e ...)])))
