;; Implementation of two-dimensional points and point operations.

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
  ;; An arbitrary maximum allowable difference in precision between floating-point numbers.
  (define EPSILON 0.000001)
  (<= (abs (- x y))
      (* EPSILON 
         (max 1.0 
              (abs x) 
              (abs y)))))

;; (point2 number number) -> vector
;; Constructs a two-dimensional point implemented as a vector.
;; (point2 1 2) -> #(1 2)

(define (point2 x y)
  (vector x y))

;; (point2-negate point2) -> point2
;; Inverts the signs of the point components. Flips point 180 degrees.
;; (point2-negate (point2 1 -2)) -> (point2 -1 2)

(define (point2-negate pt)
  (let ([x (vector-ref pt 0)]
        [y (vector-ref pt 1)])
    (point2 (- x) (- y))))

;; (point2-distance pair pair) -> number
;; Calculates the distance between two points.
;; (point2-distance (point2 3 0) (point2 2 0)) -> 1

(define (point2-distance p1 p2)
  (let ([x1 (vector-ref p1 0)]
        [y1 (vector-ref p1 1)]
        [x2 (vector-ref p2 0)]
        [y2 (vector-ref p2 1)])
    (hypotenuse (- x2 x1) (- y2 y1))))

;; (point2-lerp point2 point2 number) -> point2
;; Interpolates a point along a line between two points.
;; (point2-lerp (point2 0 10) (point2 8 -4) -1) -> (point2 -8 24)

(define (point2-lerp p1 p2 t)
  (let* ([x1 (vector-ref p1 0)]
         [y1 (vector-ref p1 1)]
         [x2 (vector-ref p2 0)]
         [y2 (vector-ref p2 1)]
         [x (+ x1 (* (- x2 x1) t))]
         [y (+ y1 (* (- y2 y1) t))])
    (point2 x y)))

;; (point2-path points) -> list
;; Returns a list of points.
;; (point2-path (point2 1 2) (point2 3 4)) -> '(#(1 2) #(3 4))

(define (point2-path . points) points)

;; (point2-path-length path) -> number
;; Computes the length of a path.
;; (point2-path-length (point2-path (point2 1 1) (point2 5 1) (point2 5 4) (point2 1 1))) -> 12

(define (point2-path-distance path)
  (let loop ([sum 0]
             [pth path])
    (if (<= (length pth) 1)
        sum
        (loop (+ sum (point2-distance (car pth) (cadr pth)))
              (cdr pth)))))

;; (point2-equal? point2 point2) -> boolean
;; Compares the components of points. Checks for equality.
;; (point2-equal? (point2 3 4) (point2 3 4)) -> #t

(define (point2-equal? p1 p2)
  (let ([x1 (vector-ref p1 0)]
        [y1 (vector-ref p1 1)]
        [x2 (vector-ref p2 0)]
        [y2 (vector-ref p2 1)])
    (and (= x1 x2)
         (= y1 y2))))

;; (point2-approx-eq? point2 point2) -> boolean
;; Checks whether floating-point components are approximately equal.
;; (point2-approx-eq? (point2 3.2 4.0) (point2 3.19999999989 4.0)) -> #t

(define (point2-approx-eq? p1 p2)
  (let ([x1 (vector-ref p1 0)]
        [y1 (vector-ref p1 1)]
        [x2 (vector-ref p2 0)]
        [y2 (vector-ref p2 1)])
    (and (approx-eq? x1 x2)
         (approx-eq? y1 y2))))

;; (approximate function) -> (function point) -> point
;; Generates approximation functions for rounding point components.
;; (define point2-round (approximate round)) -> (point2-round (point2 1.3 1.7)) -> #(1.0 2.0)

(define (approximate operation)
  (lambda (pt)
    (let ([x (vector-ref pt 0)]
          [y (vector-ref pt 1)])
      (point2 (operation x) (operation y)))))

(define point2-round (approximate round))
(define point2-ceil (approximate ceiling))
(define point2-floor (approximate floor))

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

(assert-equal (point2 1 2)
              #(1 2))

(assert-equal (point2-negate (point2 1 -2))
              (point2 -1 2))

(assert-equal (point2-distance (point2 3 0) (point2 2 0))
              1)

(assert-equal (point2-lerp (point2 0 10) (point2 8 -4) -1)
              #(-8 24))

(assert-equal (point2-path (point2 1 2) (point2 3 4))
              '(#(1 2) #(3 4)))

(assert-equal (point2-path-distance (point2-path (point2 1 1) (point2 5 1) (point2 5 4) (point2 1 1)))
              12)

(assert-equal (point2-equal? (point2 3 4) (point2 3 4))
              #t)

(assert-equal (point2-approx-eq? (point2 3.2 4.0) (point2 3.19999999989 4.0))
              #t)

(assert-equal (point2-round (point2 1.3 1.7))
              #(1.0 2.0))

(assert-equal (point2-ceil (point2 1.3 1.7))
              #(2.0 2.0))

(assert-equal (point2-floor (point2 1.3 1.7))
              #(1.0 1.0))
