;; (filter predicate list) -> list
;; Returns list whose elements satisfy the given predicate function.
;; (filter odd? '(1 2 3 4 5 6 7)) -> '(1 3 5 7)

(define (filter test xs)
  (cond [(null? xs) '()]
        [(test (car xs))
         (cons (car xs)
               (filter test (cdr xs)))]
        [else  (filter test (cdr xs))]))

;; === alternate ===

(define filter
  (lambda (test xs)
    (fold-right (lambda (x acc)
                  (if (test x)
                      (cons x acc)
                      acc))
                '()
                xs)))
