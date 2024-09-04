;; (filter predicate list) -> list
;; Returns list whose elements satisfy the given predicate function.
;; (filter odd? '(1 2 3 4 5 6 7)) -> '(1 3 5 7)

(define filter
  (lambda (test xs)
    (fold-right (lambda (x accum)
                  (if (test x)
                      (cons x accum)
                      accum))
                '()
                xs)))
