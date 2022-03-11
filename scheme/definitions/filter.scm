;; (filter predicate list) -> list
;; Returns list whose elements satisfy the given predicate function.
;; (filter odd? '(1 2 3 4 5 6 7)) -> '(1 3 5 7)

(define (filter predicate sequence)
  (cond [(null? sequence) null]
        [(predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence)))]
        [else (filter predicate (cdr sequence))]))
