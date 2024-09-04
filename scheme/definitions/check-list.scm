;; (list? any) -> boolean
;; Checks if object is a proper, non-cyclic list.
;; (list? '(x y z))   -> #t
;; (list? '(x y . z)) -> #f
;; (list? x)          -> #f
(define list?
  (lambda (xs)
    (let loop ([hare xs] [tortoise xs])
      (if (pair? hare)
          (let ([hare (cdr hare)])
            (if (pair? hare)
                (and (not (eq? hare tortoise))
                     (loop (cdr hare)
                           (if (pair? tortoise)
                               (cdr tortoise)
                               hare)))
                (null? hare)))
          (null? hare)))))
