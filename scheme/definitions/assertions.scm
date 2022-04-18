;; (assert-eq expression value) -> boolean
;; Asserts expression evaluates to value.
;; (assert-eq (list 1 2 3) '(1 2 3)) -> true

(define (assert-eq expression value)
  (equal? expression value))

;; (assert-ne expression value) -> boolean
;; Asserts expression does not evaluate to value.
;; (assert-ne '(1 (2 3)) '(1 2 3)) -> true

(define (assert-ne expression value)
  (not (equal? expression value)))
