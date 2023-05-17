;; bounded = abs(f(x)) <= m where m = real number

;; Side Note: `max` and `min` are not truly bounded. Neither negative 
;; nor positive infinity are real numbers.

(define bounded
  (lambda (compare identity)
    (lambda (xs)
      (fold-left (lambda (old new) 
                   (if (compare old new) 
                       old 
                       new))
                 identity
                 xs))))

;; (max (list number)) -> number
;; Returns the largest number in a list.
;; (max '(1/3 2.0 -3)) -> 2.0

(define max (bounded > -inf.0))

;; (min (list number)) -> number
;; Returns the smallest number in a list.
;; (min '(1/3 2.0 -3)) -> -3

(define min (bounded < +inf.0))
