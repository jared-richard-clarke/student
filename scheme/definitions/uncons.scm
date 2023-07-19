;; (uncons pair) -> (values any any)
;; Inverse operation of cons. Returns the car
;; and cdr of a pair.
;; (uncons '(7 11)) -> (values 7 '(11))

(define uncons
  (lambda (xs)
    (values (car xs) (cdr xs))))
