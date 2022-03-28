;; Functional implementation of vectors and operations in linear vector space.
;; Vector renamed 2d-vector to avoid namespace clash with vector function.

;; (hypotenuse number number) -> number
;; Computes the longest side of a right triangle.
;; (hypotenuse 3 4) -> 5

(define (hypotenuse x y)
  (sqrt (+ (sqr x) (sqr y))))

;; (2d-vector number number) -> pair
;; Returns two-dimensional coordinates as a number pair.
;; (2d-vector 3 4) -> '(3 . 4)

(define (2d-vector x y)
  (cons x y))

;; (add 2d-vector 2d-vector) -> 2d-vector
;; Returns the sum of two vectors.
;; (add (2d-vector 1 2) (2d-vector 1 2)) -> '(2 . 4)

(define (add v1 v2)
  (let ([x1 (car v1)]
        [y1 (cdr v1)]
        [x2 (car v2)]
        [y2 (cdr v2)])
    (cons (+ x1 x2) (+ y1 y2))))

;; (scale 2d-vector number) -> vect
;; Returns a vector multiplied by a number.
;; (scale (2d-vector 1 2) 2) -> '(2 . 4)

(define (scale v factor)
  (let ([x (car v)]
        [y (cdr v)])
    (cons (* factor x) (* factor y))))

;; (magnitude 2d-vector) -> number
;; Returns the magnitude of a 2d vector.
;; (magnitude (2d-vector 3 4)) -> 5

(define (magnitude v)
  (let ([x (car v)]
        [y (cdr v)])
    (hypotenuse x y)))

;; (unit-vector 2d-vector) -> 2d-vector
;; Computes the unit vector for a given vector.
;; Unit vectors have magnitudes of 1.
;; (unit-vector (2d-vector 3 4)) -> '(3/5 . 4/5) -> mag 1
;; (unit-vector (2d-vector 10 10)) -> '(0.7071067811865475 . 0.7071067811865475)
;; -> mag 0.9999999999999999

(define (unit-vector v)
  (let* ([x (car v)]
         [y (cdr v)]
         [magnitude (hypotenuse x y)])
    (cons (/ x magnitude) (/ y magnitude))))
