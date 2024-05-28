;; A "let" macro wherein a fender verifies
;; that certain subforms are identifiers

(define-syntax let
  (lambda (x)
    (define (ids? xs)
      (or (null? xs)
          (and (identifier? (car xs))
               (ids? (cdr xs)))))
    (syntax-case x ()
      [(_ ((i e) ...) b1 b2 ...)
       (ids? (syntax (i ...)))
       (syntax ((lambda (i ...) b1 b2 ...) e ...))])))
