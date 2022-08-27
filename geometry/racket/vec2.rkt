#lang racket

;; Provides two-dimensional vectors and operations in linear vector space.

(provide vec2
         vec2-approx-eq?
         vec2-add
         vec2-sub
         vec2-negate
         vec2-sum
         vec2-mag
         vec2-scale
         vec2-dot
         vec2-distance
         vec2-lerp
         vec2-normalize
         vec2-round)

(require
  "utils.rkt"
  "tests.rkt")

;; (vec2 number number) -> (vector number number)
;; Returns two-dimensional coordinates as a vector of two numbers.
;; (vec2 3 4) -> #(3 4)

(define (vec2 x y)
  (vector x y))

;; (vec2-approx-eq? (vector number number) (vector number number)) -> boolean
;; Checks whether floating-point vector components are approximately equal.
;; Comparisons made left to right.
;; (vec2-approx-eq? (vec2 3.2 4.0) (vec2 3.19999999989 4.0)) -> #t

(define (vec2-approx-eq? v1 v2)
  (let ([x1 (vector-ref v1 0)]
        [y1 (vector-ref v1 1)]
        [x2 (vector-ref v2 0)]
        [y2 (vector-ref v2 1)])
    (and (approx-eq? x1 x2)
         (approx-eq? y1 y2))))

;; (arithmetic function) -> (function (vector number number) (vector number number)) -> (vector number number)
;; Creates functions that perform an arithmetic operation over two vectors.
;; (define vec2-add (arithmetic +)) -> (vec2-add (vec2 1 2) (vec2 3 4)) -> #(4 5)

(define (arithmetic operation)
  (lambda (v1 v2)
    (let ([x1 (vector-ref v1 0)]
          [y1 (vector-ref v1 1)]
          [x2 (vector-ref v2 0)]
          [y2 (vector-ref v2 1)])
      (vec2 (operation x1 x2) (operation y1 y2)))))

;; (vec2-add (vector number number) (vector number number)) -> (vector number number)
;; Returns the sum of two, two-dimensional vectors.
;; (vec2-add (vec2 3 4) (vec2 7 11)) -> #(10 15)

(define vec2-add (arithmetic +))

;; (vec2-sub (vector number number) (vector number number)) -> (vector number number)
;; Returns the difference of two, two-dimensional vectors.
;; (vec2-sub (vec2 3 4) (vec2 7 11)) -> #(-4 -7)

(define vec2-sub (arithmetic -))

;; (vec2-negate (vector number number)) -> (vector number number)
;; Inverts the signs of the vector components. Flips the vector 180 degrees.
;; (vec2-negate (vec2 3 4)) -> (vec2 -3 -4)

(define (vec2-negate vec)
  (let ([x (vector-ref vec 0)]
        [y (vector-ref vec 1)])
    (vec2 (- x) (- y))))

;; (vec2-sum (vector number number) ...) -> (vector number number)
;; Returns the sum of a series of vectors.
;; (vec2-sum (vec2 1 2) (vec2 1 2) (vec2 3 4)) -> #(5 8)
;; (vec2-sum) -> #(0 0)

(define (vec2-sum . vecs)
    (cond
      [(= (length vecs) 0) (vec2 0 0)]
      [(= (length vecs) 1) (car vecs)]
      [else (foldl (lambda (vec accum)
                     (let ([x1 (vector-ref accum 0)]
                           [y1 (vector-ref accum 1)]
                           [x2 (vector-ref vec 0)]
                           [y2 (vector-ref vec 1)])
                       (vec2 (+ x1 x2) (+ y1 y2))))
                   (car vecs)
                   (cdr vecs))]))

;; (vec2-mag (vector number number)) -> number
;; Returns the magnitude of a two-dimensional vector.
;; (vec2-mag (vec2 3 4)) -> 5

(define (vec2-mag vec)
  (let ([x (vector-ref vec 0)]
        [y (vector-ref vec 1)])
    (hypotenuse x y)))

;; (vec2-scale (vector number number) number) -> (vector number number)
;; Returns a vector multiplied by a number.
;; (vec2-scale (vec2 1 2) 2) -> #(2 4)

(define (vec2-scale vec scalar)
  (let ([x (vector-ref vec 0)]
        [y (vector-ref vec 1)])
    (vec2 (* scalar x) (* scalar y))))

;; (vec2-dot (vector number number) (vector number number)) -> number
;; Returns the dot product of two vectors.
;; (vec2-dot (vec2 1 2) (vec2 3 4)) -> 11

(define (vec2-dot v1 v2)
  (let ([x1 (vector-ref v1 0)]
        [y1 (vector-ref v1 1)]
        [x2 (vector-ref v2 0)]
        [y2 (vector-ref v2 1)])
    (+ (* x1 x2) (* y1 y2))))

;; (vec2-distance (vector number number) (vector number number)) -> number
;; Returns the distance between two, two-dimensional vectors.
;; (vec2-distance (vec2 8 0) (vec2 1 0)) -> 7

(define (vec2-distance v1 v2)
  (let ([x1 (vector-ref v1 0)]
        [y1 (vector-ref v1 1)]
        [x2 (vector-ref v2 0)]
        [y2 (vector-ref v2 1)])
    (hypotenuse (- x2 x1) (- y2 y1))))

;; (vec2-lerp vec2 vec2 number) -> vec2
;; Interpolates a vector point along a line between two vector points.
;; (vec2-lerp 1/2 (vec2 0 0) (vec2 10 0)) -> (vec2 5 0)

(define (vec2-lerp t v1 v2)
  (let* ([x1 (vector-ref v1 0)]
         [y1 (vector-ref v1 1)]
         [x2 (vector-ref v2 0)]
         [y2 (vector-ref v2 1)]
         [x (+ x1 (* (- x2 x1) t))]
         [y (+ y1 (* (- y2 y1) t))])
    (vec2 x y)))

;; (vec2-normalize (vector number number)) -> (vector number number)
;; Returns the unit vector of a two-dimensional vector.
;; (vec2-normalize (vec2 3 4)) -> #(3/5 4/5)

(define (vec2-normalize vec)
  (let ([m (vec2-mag vec)]
        [x (vector-ref vec 0)]
        [y (vector-ref vec 1)])
    (vec2 (/ x m) (/ y m))))

;; (round (vector number number)) -> (vector number number)
;; Rounds the vector components.
;; (vec2-round (vec2 1.3 1.7)) -> #(1.0 2.0)

(define (vec2-round vec)
  (let ([x (vector-ref vec 0)]
        [y (vector-ref vec 1)])
    (vec2 (round x) (round y))))

;; === unit tests ===

(assert-equal (hypotenuse 3 4)
              5)

(assert-equal (vec2 3 4)
              #(3 4))

(assert-equal (vec2-approx-eq? (vec2 3.2 4.0) (vec2 3.19999999989 4.0))
              #t)

(assert-equal (vec2-add (vec2 3 4) (vec2 7 11))
              #(10 15))

(assert-equal (vec2-sub (vec2 3 4) (vec2 7 11))
              #(-4 -7))

(assert-equal (vec2-sum (vec2 3 4) (vec2 1 2) (vec2 1 2))
              #(5 8))

(assert-equal (vec2-negate (vec2 -3 -4))
              #(3 4))

(assert-equal (vec2-mag (vec2 3 4))
              5)

(assert-equal (vec2-scale (vec2 1 2) 2)
              #(2 4))

(assert-equal (vec2-dot (vec2 1 2) (vec2 3 4))
              11)

(assert-equal (vec2-distance (vec2 8 0) (vec2 1 0))
              7)

(assert-equal (vec2-lerp 1/2 (vec2 0 0) (vec2 10 0))
              (vec2 5 0))

;; rational division
(assert-equal (vec2-normalize (vec2 3 4))
              #(3/5 4/5))

;; float division
(assert-equal (vec2-normalize (vec2 3.0 4.0))
              #(0.6 0.8))

(assert-equal (vec2-round (vec2 1.3 1.7))
              #(1.0 2.0))
