;; (vector-count function vector vectors ...) -> number
;; Counts the number of elements from one or more
;; vectors that satisfy the given predicate.
;; (vector-count (lambda (x y) (and (< x 3) (< y 3))) '#(1 2) '#(-2 0 4)) -> 2

(define vector-count
  (lambda (predicate v . vs)
    (apply vector-fold-left
           (lambda (counter . x)
             (+ counter (if (apply predicate x) 1 0)))
           0
           v
           vs)))
