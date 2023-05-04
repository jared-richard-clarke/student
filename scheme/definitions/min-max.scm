(define compare
  (lambda (op id)
    (lambda (xs)
      (fold-left (lambda (old new) 
                   (if (op old new) 
                       old 
                       new))
                 id
                 xs))))

;; (max (list number)) -> number
;; Returns the largest number in a list.
;; (max '(1/3 2.0 -3)) -> 2.0

(define max (compare > -inf.0))

;; (min (list number)) -> number
;; Returns the smallest number in a list.
;; (min '(1/3 2.0 -3)) -> -3

(define min (compare < +inf.0))
