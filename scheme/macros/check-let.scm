;; A "let" macro wherein a fender verifies that
;; certain subforms of an input form are identifiers

(define-syntax let
  (lambda (x)
    (define ids?
      (lambda (ls)
        (or (null? ls)
            (and (identifier? (car ls))
                 (ids? (cdr ls))))))
    (syntax-case x ()
      [(_ ((i e) ...) b1 b2 ...)
       (ids? (syntax (i ...)))
       (syntax ((lambda (i ...) b1 b2 ...) e ...))])))
