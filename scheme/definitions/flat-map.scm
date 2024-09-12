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

;; (flatten (list any)) -> (list any)
;; Flattens a nested list to its base elements.
;; (flatten '(((1 2) 3) 4)) -> '(1 2 3 4)
(define flatten
  (lambda (xs)
    (let ([id (lambda (x) x)])
      (flat-map id xs))))

;; === Side Note ===
;; An alternative definition of "flat-map" is the monadic definition of "bind":
;; flat-map: [a] -> (a -> [b]) -> [b]

(define flat-map
  (lambda (fn xs)
	  (fold-right append '() (map fn xs))))

;; -- equivalent ->

(define flat-map
  (lambda (fn xs)
    (if (null? xs)
        '()
        (append (fn (car xs)) (flat-map fn (cdr xs))))))
