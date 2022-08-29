;; (GCD integer integer) -> integer
;; The greatest common divisor of x and y is the largest integer that divides both x and y with no remainder.
;; (GCD 10 5) -> 5

(define (GCD x y)
  (if (= y 0)
      x
      (GCD y (remainder x y))))

;; (LCM integer integer) -> integer
;; Returns the least common multiple — the smallest integer that is divisible by "x" and "y".
;; (LCM 10 5) -> 10

(define (LCM x y)
    (/ (* x y) (GCD x y)))
