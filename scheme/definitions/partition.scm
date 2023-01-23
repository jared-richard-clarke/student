;; (partition function (list any)) -> (list (list any) (list any))
;; Inputs a list and predicate and returns a pair of lists wherein 
;; the first list contains all the elements that satisfy the predicate
;; and the second list contains all the elements that don't.
;; (partition (lambda (x) (< x 5)) '(1 2 3 4 5 6 7 8 9 10)) -> '((1 2 3 4) (5 6 7 8 9 10))

(define (partition pred xs)
  (let ([yes (filter pred xs)]
        [no  (filter (lambda (x) (not (pred x))) xs)])
    (list yes no)))
