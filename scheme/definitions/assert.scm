;; (assert expression value) -> boolean
;; Asserts expression evaluates to value.
;; (assert (list 1 2 3) '(1 2 3)) -> true

(define (assert expression value)
  (equal? expression value))
