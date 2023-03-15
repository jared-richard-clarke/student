;; (partition function (list any)) -> (list any) (list any)
;; Inputs a list and predicate and returns two lists wherein 
;; the first list contains all the elements that satisfy the predicate
;; and the second list contains all the elements that don't.
;; (partition even? '(1 2 3 4 5 6 7 8 9 10)) -> (2 4 6 8 10) (1 3 5 7 9)

(define (partition test xs)
  (let ([yes (filter test xs)]
        [no  (filter (lambda (x) (not (test x))) xs)])
    (values yes no)))
