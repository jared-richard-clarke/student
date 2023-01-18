;; (flat-map function (list any)) -> (list any)
;; Flattens a nested list to its base elements and transforms
;; those elements with an arbitrary function.
;; (flat-map (lambda (x) (+ x 2)) '(1 ((2) (3)))) -> '(3 4 5)

(define flat-map
  (lambda (fn xs)
    (cond [(null? xs) '()]
          [(not (pair? xs)) (list (fn xs))]
          [else (append (flat-map fn (car xs))
                        (flat-map fn (cdr xs)))])))
