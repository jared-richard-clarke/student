(import (rnrs base)
        (vectors)
        (utils))

;; === unit tests ===

(assert-equal (hypotenuse 3 4)
            5)

(assert-equal (vec2 3 4)
            '#(3 4))

(assert-equal (vec2-approx-eq? (vec2 3.2 4.0) (vec2 3.19999999989 4.0))
            #t)

(assert-equal (vec2-add (vec2 3 4) (vec2 7 11))
            '#(10 15))

(assert-equal (vec2-sub (vec2 3 4) (vec2 7 11))
            '#(-4 -7))

(assert-equal (vec2-sum (vec2 3 4) (vec2 1 2) (vec2 1 2))
            '#(5 8))

(assert-equal (vec2-negate (vec2 -3 -4))
            '#(3 4))

(assert-equal (vec2-mag (vec2 3 4))
            5)

(assert-equal (vec2-scale (vec2 1 2) 2)
            '#(2 4))

(assert-equal (vec2-dot (vec2 1 2) (vec2 3 4))
            11)

(assert-equal (vec2-distance (vec2 8 0) (vec2 1 0))
            7)

(assert-equal (vec2-lerp 1/2 (vec2 0 0) (vec2 10 0))
            (vec2 5 0))

;; rational division
(assert-equal (vec2-normalize (vec2 3 4))
            '#(3/5 4/5))

;; float division
(assert-equal (vec2-normalize (vec2 3.0 4.0))
            '#(0.6 0.8))

(assert-equal (vec2-round (vec2 1.3 1.7))
            '#(1.0 2.0))
