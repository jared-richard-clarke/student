(define compare
  (lambda (operation identity)
    (lambda (xs)
      (fold-left (lambda (old new) (if (operation old new) old new))
                 identity
                 xs))))

;; (max (list number)) -> number
;; Returns the largest number in a list.
;; (max '(1/3 2.0 -3)) -> 2.0

(define max (compare > -inf.0))

;; (min (list number)) -> number
;; Returns the smallest number in a list.
;; (min '(1/3 2.0 -3)) -> -3

(define min (compare < +inf.0))
