;; Implementation of two-dimensional points and point operations.

;; (hypotenuse number number) -> number
;; Returns the square root of the sum of the squares of its arguments.
;; (hypotenuse 3 4) -> 5

(define (hypotenuse . numbers)
  (sqrt (foldl (lambda (number accum)
                 (+ accum (sqr number)))
               0
               numbers)))

;; (point number number) -> vector
;; Constructs a two-dimensional point represented as a vector.
;; (point 1 2) -> #(1 2)

(define (point x y)
  (vector x y))

;; ORIGIN: point of origin.

(define ORIGIN (point 0 0))

;; EPSILON
;; The maximum allowable difference in precision between two floating-point coordinates.

(define EPSILON 0.000001)

;; (segment pair pair) -> number
;; Calculates the distance between two points.
;; (segment (point 3 0) (point 2 0)) -> 1

(define (segment p1 p2)
  (let ([x1 (vector-ref p1 0)]
        [y1 (vector-ref p1 1)]
        [x2 (vector-ref p2 0)]
        [y2 (vector-ref p2 1)])
    (hypotenuse (- x2 x1) (- y2 y1))))

;; (path points) -> list
;; Returns a list of points.
;; (path (point 1 2) (point 3 4)) -> '(#(1 2) #(3 4))

(define (path . points) points)

;; (path-length path) -> number
;; Computes the length of a path.
;; (path-length (path (point 1 1) (point 5 1) (point 5 4) (point 1 1))) -> 12

(define (path-length path)
  (let loop ([sum 0]
             [pth path])
    (if (<= (length pth) 1)
        sum
        (loop (+ sum (segment (car pth) (cadr pth)))
              (cdr pth)))))

;; (approximate function) -> (function point) -> point
;; Generates approximation functions for simplifying point components.
;; (define point-round (approximate round)) -> (point-round (point 1.3 1.7)) -> #(1.0 2.0)

(define (approximate operation)
  (lambda (pt)
    (let ([x (vector-ref pt 0)]
          [y (vector-ref pt 1)])
      (point (operation x) (operation y)))))

(define point-round (approximate round))
(define point-ceiling (approximate ceiling))
(define point-floor (approximate floor))

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

(assert-equal (point 1 2)
              #(1 2))

(assert-equal (segment (point 3 0) (point 2 0))
              1)

(assert-equal (path (point 1 2) (point 3 4))
              '(#(1 2) #(3 4)))

(assert-equal (path-length (path (point 1 1) (point 5 1) (point 5 4) (point 1 1)))
              12)

(assert-equal (point-round (point 1.3 1.7))
              #(1.0 2.0))

(assert-equal (point-ceiling (point 1.3 1.7))
              #(2.0 2.0))

(assert-equal (point-floor (point 1.3 1.7))
              #(1.0 1.0))
