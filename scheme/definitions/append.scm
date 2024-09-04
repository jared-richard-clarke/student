;; (append (list any) ...) -> (list any)
;; Combines elements of multiple lists into a single list.
;; (append '(1 2) '(3 4) '(5 6 7)) -> '(1 2 3 4 5 6 7)

(define append
  (lambda xys
    (let ([++ (lambda (xs ys)
                (if (null? ys)
                    xs
                    (fold-right cons ys xs)))])
      (fold-right ++ '() xys))))
