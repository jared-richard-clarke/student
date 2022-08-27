#lang racket


(provide hypotenuse approx-eq?)

;; (hypotenuse number number) -> number
;; Returns the square root of the sum of the squares of its arguments.
;; (hypotenuse 3 4) -> 5

(define (hypotenuse . numbers)
  (sqrt (foldl (lambda (number accum)
                 (+ accum (sqr number)))
               0
               numbers)))

;; (approx-eq? number number) -> boolean
;; Tests for approximate equality between two floating-point numbers within an absolute
;; or relative tolerance of EPSILON. An absolute tolerance is used for values
;; less than or equal to 1.0. A relative tolerance is used for larger values.
;; (approx-eq? 0.2 0.19999999) -> #t

(define (approx-eq? x y)
  (let ([EPSILON 0.000001]) ;; <- arbitrary maximum allowable difference
    (<= (abs (- x y))
        (* EPSILON 
           (max 1.0 
                (abs x) 
                (abs y))))))
