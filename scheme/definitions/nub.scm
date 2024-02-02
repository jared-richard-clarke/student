;; (nub list) -> list
;; Removes duplicate elements from a list.
;; (nub '(1 2 2 3 4 4 1)) -> '(1 2 3 4)

(define nub
  (let ([not-eq? (lambda (x)
                   (lambda (y)
                     (not (equal? x y))))])
    (lambda (xs)
      (if (null? xs)
          '()
          (let ([x  (car xs)]
                [xs (cdr xs)])
            (cons x (nub (filter (not-eq? x) xs))))))))
