;; (flatten list) -> list
;; Flattens a nested list to its base elements.
;; (flatten '(((1 2) 3) 4)) -> '(1 2 3 4)
(define flatten
  (lambda (xs)
    (cond
      [(null? xs) '()]
      [(not (pair? xs)) (list xs)]
      [else (append (flatten (car xs))
                    (flatten (cdr xs)))])))
