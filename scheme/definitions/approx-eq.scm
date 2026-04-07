;; (approx-eq? number number) -> boolean
;; Tests for equality between two numbers. If those two numbers are unequal,
;; tests for approximate equality within a tolerance of 1e-7.
;; (approx-eq? 0.2 0.19999999) -> #t

(define approx-eq?
  (lambda (x y)
    (let ([TOLERANCE 1e-7])
      (or (= x y)
          (< (/ (abs (- x y))
                (max (abs x) (abs y)))
             TOLERANCE)))))
