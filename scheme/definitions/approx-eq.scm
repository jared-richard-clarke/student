;; EPSILON
;; The maximum allowable difference in precision between floating-point numbers.

(define EPSILON 0.000001)

;; (approx-eq? number number) -> boolean
;; Tests for approximate equality between two floating-point numbers within an absolute
;; or relative tolerance of EPSILON. An absolute tolerance is used for values
;; less than or equal to 1.0. A relative tolerance is used for larger values.
;; (approx-eq? 0.2 0.19999999) -> #t

(define (approx-eq? x y)
  (<= (abs (- x y))
      (* EPSILON
         (max 1.0
              (abs x)
              (abs y)))))
