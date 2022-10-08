(import (rnrs base)
        (vectors)
        (utils))

;; === unit tests ===

(assert-equal (hypotenuse 3 4)
              5)

;; Vectors

(assert-equal (vec2 3 4)
              '#(3 4))

(assert-equal (vec3 3 4 1)
              '#(3 4 1))

(assert-equal (vec2? (vec2 3 4))
              #t)

(assert-equal (vec3? '#(3 4 1))
              #t)

;; Addition

(assert-equal (vec-add (vec2 3 4) (vec2 7 11))
              '#(10 15))

(assert-equal (vec-add (vec3 3 4 5) (vec3 1 2 3))
              '#(4 6 8))

;; Subtraction

(assert-equal (vec-sub (vec2 3 4) (vec2 7 11))
              '#(-4 -7))

(assert-equal (vec-sub (vec3 3 4 5) (vec3 1 2 3))
              '#(2 2 2))

;; Negation

(assert-equal (vec-neg (vec2 -3 -4))
              '#(3 4))

(assert-equal (vec-neg (vec2 0 0))
              '#(0 0))

(assert-equal (vec-neg (vec3 3 4 -5))
              '#(-3 -4 5))

(assert-equal (vec-neg (vec3 0 0 0))
              '#(0 0 0))

;; Absolute

(assert-equal (vec-abs (vec2 -3 -4))
              '#(3 4))

(assert-equal (vec-abs (vec3 -3 -4 -5))
              '#(3 4 5))

;; Inversion

(assert-equal (vec-invert (vec2 2 2))
              '#(1/2 1/2))

(assert-equal (vec-invert (vec2 1/2 1/2))
              '#(2 2))

(assert-equal (vec-invert (vec3 3 4 5))
              '#(1/3 1/4 1/5))

(assert-equal (vec-invert (vec3 1/3 1/4 1/5))
              '#(3 4 5))

;; Summation

(assert-equal (vec-sum (vec2 3 4) (vec2 1 2) (vec2 1 2))
              '#(5 8))

(assert-equal (vec-sum (vec2 3 4))
              '#(3 4))

(assert-equal (vec-sum (vec3 3 4 5) (vec3 1 2 3) (vec3 1 1 1))
              '#(5 7 9))

(assert-equal (vec-sum (vec3 1 2 3))
              '#(1 2 3))

;; Magnitude

(assert-equal (vec-mag (vec2 3 4))
              5)

(assert-equal (vec-mag (vec3 3 4 5))
              7.0710678118654755)

;; Scale

(assert-equal (vec-scale (vec2 1 2) 2)
              '#(2 4))

(assert-equal (vec-scale (vec3 1 2 3) 2)
              '#(2 4 6))

;; Dot Product

(assert-equal (vec-dot (vec2 1 2) (vec2 3 4))
              11)

(assert-equal (vec-dot (vec3 1 2 3) (vec3 3 4 5))
              26)

;; Distance

(assert-equal (vec-dist (vec2 8 0) (vec2 1 0))
              7)

(assert-equal (vec-dist (vec3 8 0 0) (vec3 1 0 0))
              7)

;; Interpolation

(assert-equal (vec-lerp (vec2 1 1) (vec2 3 2) 0.5)
              (vec2 2.0 1.5))

(assert-equal (vec-lerp (vec3 3 4 5) (vec3 1 2 3) 0.5)
              '#(2.0 3.0 4.0))

;; Normalize

;; rational division
(assert-equal (vec-normalize (vec2 3 4))
              '#(3/5 4/5))

;; floating-point division
(assert-equal (vec-normalize (vec2 3.0 4.0))
              '#(0.6 0.8))

;; Rounding

(assert-equal (vec-round (vec2 1.3 1.7))
              '#(1.0 2.0))

(assert-equal (vec-round (vec3 2.5 1.75 0.25))
              '#(2.0 2.0 0.0))
