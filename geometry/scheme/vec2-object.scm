;; Object implementation of vectors and operations in linear vector space.

;; (hypotenuse number number) -> number
;; Returns the square root of the sum of the squares of its arguments.
;; (hypotenuse 3 4) -> 5

(define (hypotenuse . numbers)
  (sqrt (foldl (lambda (number accum)
                 (+ accum (sqr number)))
               0
               numbers)))

;; (vec2 number number) -> vec2
;; Creates two-dimensional vector object implemented as a function. Captures values within its closure.
;; (define v (vec2 3 4))
;; (v 'x) -> 3
;; (v 'y) -> 4
;; (v 'point) -> #(3 4)
;; (v 'type) -> 'vec2
;; (v 'magnitude) -> 5

(define (vec2 x y)
  ;; === properties ===
  (let* ([px x]
         [py y]
         [point (vector x y)]
         [type 'vec2]
         [magnitude (hypotenuse x y)])
    ;; === interface ===
    (lambda (message)
      (cond [(eq? message 'x) px]
            [(eq? message 'y) py]
            [(eq? message 'point) point]
            [(eq? message 'type) type]
            [(eq? message 'magnitude) magnitude]
            [else (error "invalid input:" message)]))))

;; I-HAT, J-HAT
;; Mutually orthogonal two-dimensional unit vectors, forming the standard basis.

(define I-HAT (vec2 1 0))
(define J-HAT (vec2 0 1))

;; EPSILON
;; The maximum allowable difference in precision between two floating-point vector coordinates.

(define EPSILON 0.000001)

;; (vec2-flip vec2) -> vec2
;; Inverts the signs of the vector components. Flips vector 180 degrees.
;; (vec2-flip (vec2 3 4)) -> (vec2 -3 -4)

(define (vec2-flip vec)
  (let ([x (vec 'x)]
        [y (vec 'y)])
    (vec2 (* x -1) (* y -1))))

;; (vec2-add vec2 vec2) -> vec2
;; Returns a two-dimensional vector that is the sum of a series of two-dimensionsal vectors.
;; (vec2-add (vec2 1 2) (vec2 3 4) (vec2 2 1)) -> (vec2 'point) -> #(6 7)

(define (vec2-add . vecs)
  (if (< (length vecs) 2)
      (car vecs)
      ;; To prevent memory consumption, add processes vec2s as a series of vectors,
      ;; converting only the sum to a vec2 function with closure. 
      (let ([sum (foldl (lambda (v1 v2)
                          (let ([x1 (vector-ref v1 0)]
                                [y1 (vector-ref v1 1)]
                                [x2 (vector-ref v2 0)]
                                [y2 (vector-ref v2 1)])
                            (vector (+ x1 x2) (+ y1 y2))))
                        #(0 0)
                        (map (lambda (v) (v 'point)) vecs))])
        (vec2 (vector-ref sum 0) (vector-ref sum 1)))))

;; (vec2-scale vec2 number) -> vec2
;; Returns a scaled two-dimensional vector that is the product of a two-dimensional vector and a number.
;; (vec2-scale (vec2 3 4) 2) -> (vec2 'point) -> #(6 8)

(define (vec2-scale vec scalar)
  (let ([x (vec 'x)]
        [y (vec 'y)])
    (vec2 (* x scalar) (* y scalar))))

;; (vec2-dot vec2 vec2) -> number
;; Computes the dot product of two two-dimensional vectors.
;; (vec2-dot (vec2 1 2) (vec2 3 4)) -> 11

(define (vec2-dot v1 v2)
  (let* ([x1 (v1 'x)]
         [y1 (v1 'y)]
         [x2 (v2 'x)]
         [y2 (v2 'y)])
    (+ (* x1 x2) (* y1 y2))))

;; (vec2-distance vec2 vec2) -> number
;; Returns the distance between two, two-dimensional vectors.
;; (vec2-distance (vec2 8 0) (vec2 1 0)) -> 7

(define (vec2-distance v1 v2)
  (let ([x1 (v1 'x)]
        [y1 (v1 'y)]
        [x2 (v2 'x)]
        [y2 (v2 'y)])
    (hypotenuse (- x2 x1) (- y2 y1))))

;; (compare operator) -> function
;; Generates functions for sequentially comparing the magnitudes of a list of two-dimensional vectors.
;; (define vec-gt? (compare >)) -> (vec-gt? (vec2 3 4) (vec2 1 2)) -> #t

(define (compare operator)
  (lambda vecs
    (apply operator
           (map (lambda (v) (v 'magnitude)) vecs))))

;; vec2 comparison functions

(define vec2-gt? (compare >))
(define vec2-lt? (compare <))
(define vec2-ge? (compare >=))
(define vec2-le? (compare <=))
(define vec2-eq? (compare =))

;; (approximate function) -> (function vec2) -> vec2
;; Generates approximation functions for simplifying vector components.
;; (define vec2-round (approximate round)) -> ((vec2-round (vec2 1.3 1.7)) 'point) -> '(1.0 . 2.0)

(define (approximate operation)
  (lambda (vec)
    (let ([x (vec 'x)]
          [y (vec 'y)])
      (vec2 (operation x) (operation y)))))

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

(assert-equal ((vec2-flip (vec2 3 4)) 'point)
              #(-3 -4))

(assert-equal ((vec2-add (vec2 1 2) (vec2 3 4) (vec2 2 1)) 'point)
              #(6 7))

(assert-equal ((vec2-scale (vec2 3 4) 2) 'point)
              #(6 8))

(assert-equal (vec2-dot (vec2 1 2) (vec2 3 4))
              11)

(assert-equal (vec2-distance (vec2 8 0) (vec2 1 0))
              7)

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

(assert-equal ((vec2-round (vec2 1.3 1.7)) 'point)
              #(1.0 2.0))

(assert-equal ((vec2-ceil (vec2 1.3 1.7)) 'point)
              #(2.0 2.0))

(assert-equal ((vec2-floor (vec2 1.3 1.7)) 'point)
              #(1.0 1.0))
