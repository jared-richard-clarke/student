;; (reverse (list any)) -> (list any)
;; Reverses elements in a list.
;; (reverse '(1 2 3)) -> '(3 2 1)

(define reverse
  (lambda (xs)
    (fold-left cons '() xs)))
