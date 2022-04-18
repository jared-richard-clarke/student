;; (assert-equal expression value) -> boolean
;; Asserts whether expression evaluates to value.
;; (assert-equal (list 1 2 3) '(1 2 3)) -> true

(define (assert-equal expression value)
  (equal? expression value))
