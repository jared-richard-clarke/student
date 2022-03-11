;; (equal? value value) -> boolean
;; Performs deep comparison — recursively if both values are lists.
;; (equal? '(1 2 (3 4)) '(1 2 (3 4))) -> #t

(define (equal? x y)
  (cond [(and (not (pair? x)) (not (pair? y)))
         (eq? x y)]
        [(and (pair? x) (pair? y))
         (and (equal? (car x) (car y))
              (equal? (cdr x) (cdr y)))]
        [else #f]))
