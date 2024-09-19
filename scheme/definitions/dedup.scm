;; (dedup list) -> list
;; Removes duplicate elements from a list.
;; (dedup '(1 2 2 3 4 4 1)) -> '(1 2 3 4)

(define dedup
  (let ([not-equal? (lambda (x)
                      (lambda (y)
                        (not (equal? x y))))])
    (lambda (xs)
      (if (null? xs)
          '()
          (let ([x  (car xs)]
                [xs (cdr xs)])
            (cons x (dedup (filter (not-equal? x) xs))))))))
