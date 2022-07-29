;; Provides cartesian coordinates and associated functions.

;; (hypotenuse number number) -> number
;; Returns the square root of the sum of the squares of its arguments.
;; (hypotenuse 3 4) -> 5

(define (hypotenuse . numbers)
  (sqrt (foldl (lambda (number accum)
                 (+ accum (sqr number)))
               0
               numbers)))

;; (pt2 number number) -> (vector number number)
;; Creates a cartesian representation of a point in two dimensions.
;; Implemented as a vector.
;; (pt2 3 4) -> #(3 4)

(define (pt2 x y)
  (vector x y))

;; (pt2-distance pt2 pt2) -> number
;; Computes the distance between two points.
;; (pt2-distance (pt2 10 0) (pt2 5 0))

(define (pt2-distance p1 p2)
  (let ([x1 (vector-ref p1 0)]
        [y1 (vector-ref p1 1)]
        [x2 (vector-ref p2 0)]
        [y2 (vector-ref p2 1)])
    (hypotenuse (- x2 x1) (- y2 y1))))

;; (pt2-lerp pt2 pt2) -> pt2
;; Interpolates a point along a line between two points.
;; (pt2-lerp (pt2 0 10) (pt2 8 -4) -1) -> (pt2 -8 24)

(define (pt2-lerp p1 p2 t)
  (let* ([x1 (vector-ref p1 0)]
         [y1 (vector-ref p1 1)]
         [x2 (vector-ref p2 0)]
         [y2 (vector-ref p2 1)]
         [x (+ x1 (* (- x2 x1) t))]
         [y (+ y1 (* (- y2 y1) t))])
    (pt2 x y)))
