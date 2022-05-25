;; Functional implementation of vectors and operations in linear vector space.

;; (hypotenuse number number) -> number
;; Returns the square root of the sum of the squares of its arguments.
;; (hypotenuse 3 4) -> 5

(define (hypotenuse . numbers)
  (sqrt (foldl (lambda (number accum)
                 (+ accum (sqr number)))
               0
               numbers)))

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

;; (vec2 number number) -> vector
;; Returns two-dimensional coordinates as a number vector.
;; (vec2 3 4) -> #(3 4)

(define (vec2 x y)
  (vector x y))

;; I-HAT, J-HAT
;; Mutually orthogonal unit vectors, forming the standard basis.

(define I-HAT (vec2 1 0))
(define J-HAT (vec2 0 1))

;; (vec2-flip vec2) -> vec2
;; Inverts the signs of the vector components. Flips the vector 180 degrees.
;; (vec2-flip (vec2 3 4)) -> (vec2 -3 -4)

(define (vec2-flip vec)
  (let ([x (vector-ref vec 0)]
        [y (vector-ref vec 1)])
    (vec2 (* x -1) (* y -1))))

;; (vec2-add vec2 ...) -> vec2
;; Returns the sum of a series of vectors.
;; (vec2-add (vec2 1 2) (vec2 1 2)) -> #(2 4)

(define (vec2-add . vecs)
  (if (= (length vecs) 1)
      (car vecs)
      (foldl (lambda (v1 v2)
               (let ([x1 (vector-ref v1 0)]
                     [y1 (vector-ref v1 1)]
                     [x2 (vector-ref v2 0)]
                     [y2 (vector-ref v2 1)])
                 (vec2 (+ x1 x2) (+ y1 y2))))
             #(0 0)
             vecs)))

;; (vec2-scale vec2 number) -> vec2
;; Returns a vector multiplied by a number.
;; (vec2-scale (vec2 1 2) 2) -> #(2 4)

(define (vec2-scale vec scalar)
  (let ([x (vector-ref vec 0)]
        [y (vector-ref vec 1)])
    (vec2 (* scalar x) (* scalar y))))

;; (vec2-dot vec2 vec2) -> number
;; Returns the dot product of two, two-dimensional vectors.
;; (vec2-dot (vec2 1 2) (vec2 3 4)) -> 11

(define (vec2-dot v1 v2)
  (let ([x1 (vector-ref v1 0)]
        [y1 (vector-ref v1 1)]
        [x2 (vector-ref v2 0)]
        [y2 (vector-ref v2 1)])
    (+ (* x1 x2) (* y1 y2))))

;; (vec2-magnitude vec2) -> number
;; Returns the magnitude of a two-dimensional vector.
;; (vec2-magnitude (vec2 3 4)) -> 5

(define (vec2-magnitude vec)
  (let ([x (vector-ref vec 0)]
        [y (vector-ref vec 1)])
    (hypotenuse x y)))

;; (vec2-distance vec2 vec2) -> number
;; Returns the distance between two, two-dimensional vectors.
;; (vec2-distance (vec2 8 0) (vec2 1 0)) -> 7

(define (vec2-distance v1 v2)
  (let ([x1 (vector-ref v1 0)]
        [y1 (vector-ref v1 1)]
        [x2 (vector-ref v2 0)]
        [y2 (vector-ref v2 1)])
    (hypotenuse (- x2 x1) (- y2 y1))))

;; (vec2-normalize vec2) -> vec2
;; Returns the unit vector of a two-dimensional vector. Divide components by magnitude
;; (vec2-normalize (vec2 3 4)) -> #(3/5 4/5)

(define (vec2-normalize vec)
  (let ([m (vec2-magnitude vec)]
        [x (vector-ref vec 0)]
        [y (vector-ref vec 1)])
    (vec2 (/ x m) (/ y m))))

;; (compare operator) -> (function vec2...) -> boolean
;; Generates functions for comparing the components of two, two-dimensional vectors.
;; Comparisons are applied left to right.
;; (define vec2-gt? (compare >)) -> (vec2-gt? (vec2 3 4) (vec2 1 2)) -> #t

(define (compare operator)
  (lambda (v1 v2)
    (let ([x1 (vector-ref v1 0)]
          [y1 (vector-ref v1 1)]
          [x2 (vector-ref v2 0)]
          [y2 (vector-ref v2 1)])
    (and (operator x1 x2)
         (operator y1 y2)))))

;; vec2 comparison functions

(define vec2-gt? (compare >))
(define vec2-lt? (compare <))
(define vec2-ge? (compare >=))
(define vec2-le? (compare <=))
(define vec2-eq? (compare =))

;; (approximate function) -> (function vec2) -> vec2
;; Generates approximation functions for simplifying vector components.
;; (define vec2-round (approximate round)) -> (vec2-round (vec2 1.3 1.7)) -> #(1.0 2.0)

(define (approximate operation)
  (lambda (vec)
    (let ([x (vector-ref vec 0)]
          [y (vector-ref vec 1)])
      (vec2 (operation x) (operation y)))))

;; vec2 rounding functions

(define vec2-round (approximate round))
(define vec2-ceil (approximate ceiling))
(define vec2-floor (approximate floor))

;; === testing ===

;; (assert-equal expression value) -> current-output-port
;; If expression does not evaluate to value, macro prints failed test to current-output-port.
;; (assert-equal (vec2 4 4) #(3 4)) ->
;; Test: (vec2 4 4)
;; Expect: #(3 4), Got: #(4 4)

(define-syntax assert-equal
  (syntax-rules ()
    [(_ expression value)
     (let ([computed-expr expression]) ;; <- prevents redundant computation
       (when (not (equal? computed-expr value))
         (printf "Test: ~a\nExpect: ~a, Got: ~a\n"
                 (quote expression) ;; <---- returns expression prior to evaluation
                 value
                 computed-expr)))]))

;; === unit tests ===

(assert-equal (hypotenuse 3 4)
              5)

(assert-equal (vec2 3 4)
              #(3 4))

(assert-equal (vec2-flip (vec2 -3 -4))
              #(3 4))

(assert-equal (vec2-add (vec2 1 2) (vec2 1 2))
              #(2 4))

(assert-equal (vec2-scale (vec2 1 2) 2)
              #(2 4))

(assert-equal (vec2-dot (vec2 1 2) (vec2 3 4))
              11)

(assert-equal (vec2-magnitude (vec2 3 4))
              5)

(assert-equal (vec2-distance (vec2 8 0) (vec2 1 0))
              7)

;; rational division
(assert-equal (vec2-normalize (vec2 3 4))
              #(3/5 4/5))

;; float division
(assert-equal (vec2-normalize (vec2 3.0 4.0))
              #(0.6 0.8))

(assert-equal (vec2-gt? (vec2 3 4) (vec2 1 2))
              #t)

(assert-equal (vec2-lt? (vec2 3 4) (vec2 1 2))
              #f)

(assert-equal (and (vec2-ge? (vec2 3 4) (vec2 3 4))
                   (vec2-ge? (vec2 3 4) (vec2 1 2)))
              #t)

(assert-equal (and (vec2-le? (vec2 3 4) (vec2 3 4))
                   (vec2-le? (vec2 1 2) (vec2 3 4)))
              #t)

(assert-equal (vec2-eq? (vec2 3 4) (vec2 3 4))
              #t)

(assert-equal (vec2-round (vec2 1.3 1.7))
              #(1.0 2.0))

(assert-equal (vec2-ceil (vec2 1.3 1.7))
              #(2.0 2.0))

(assert-equal (vec2-floor (vec2 1.3 1.7))
              #(1.0 1.0))
