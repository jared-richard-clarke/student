(import (rnrs base)
        (vectors)
        (utils))

;; === unit tests ===

(assert-equal (hypotenuse 3 4)
              5)

(assert-equal (vec2 3 4)
              '#(3 4))

(assert-equal (vec3 3 4 1)
              '#(3 4 1))

(assert-equal (vec3? '#(3 4 1))
              #t)

(assert-equal (vec2? (vec2 3 4))
              #t)

(assert-equal (vec-add (vec2 3 4) (vec2 7 11))
              '#(10 15))

(assert-equal (vec-sub (vec2 3 4) (vec2 7 11))
              '#(-4 -7))

(assert-equal (vec-neg (vec2 -3 -4))
              '#(3 4))

(assert-equal (vec-invert (vec2 2.0 2.0))
              '#(0.5 0.5))

(assert-equal (vec-invert (vec2 0.5 0.5))
              '#(2.0 2.0))

(assert-equal (vec-sum (vec2 3 4) (vec2 1 2) (vec2 1 2))
              '#(5 8))

(assert-equal (vec-mag (vec2 3 4))
              5)

(assert-equal (vec-scale (vec2 1 2) 2)
              '#(2 4))

(assert-equal (vec-dot (vec2 1 2) (vec2 3 4))
              11)

(assert-equal (vec-dist (vec2 8 0) (vec2 1 0))
              7)

(assert-equal (vec-lerp 1/2 (vec2 0 0) (vec2 10 0))
              (vec2 5 0))

;; rational division
(assert-equal (vec-normalize (vec2 3 4))
              '#(3/5 4/5))

;; floating-point division
(assert-equal (vec-normalize (vec2 3.0 4.0))
              '#(0.6 0.8))

(assert-equal (vec-round (vec2 1.3 1.7))
              '#(1.0 2.0))
