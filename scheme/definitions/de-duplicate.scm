;; (de-duplicate list) -> list
;; Removes duplicate elements from a list.
;; (de-duplicate '(1 2 2 3 4 4 1)) -> '(1 2 3 4)

(define de-duplicate
  (let ([not-eq? (lambda (x)
                   (lambda (y)
                     (not (equal? x y))))])
    (lambda (xs)
      (if (null? xs)
          '()
          (let ([x  (car xs)]
                [xs (cdr xs)])
            (cons x (de-duplicate (filter (not-eq? x) xs))))))))
