(define order
  (lambda (compare identity)
    (lambda (xs)
      (fold-left (lambda (old new) (if (compare old new) old new))
                 identity
                 xs))))

;; (max (list number)) -> number
;; Returns the maximum number of a list.
;; (max '(1/3 2.0 -3)) -> 2.0

(define max (order > -inf.0))

;; (min (list number)) -> number
;; Returns the minimum number of a list.
;; (min '(1/3 2.0 -3)) -> -3

(define min (order < +inf.0))
