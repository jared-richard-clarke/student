;; (gcd integer integer) -> integer
;; Returns the greatest common divisor of two integers.
;; (gcd 10 5) -> 5

(define (gcd x y)
  (if (= y 0)
      x
      (gcd y (remainder x y))))

;; (gcds integer ...)
;; Returns the greatest common divisor of a variable number of integers.
;; (gcds 10 8 4) -> 2

(define (gcds . xs)
  (fold-left gcd 0 xs))

;; (lcm integer integer) -> integer
;; Returns the least common multiple — the smallest integer that is divisible by "x" and "y".
;; (lcm 10 5) -> 10

(define (lcm x y)
  (/ (* x y) (gcd x y)))

;; (lcms integer ...)
;; Returns the least common multiple of a variable number of integers
;; (lcms 10 5 3) -> 30

(define (lcms . xs)
  (fold-left lcm 1 xs))
