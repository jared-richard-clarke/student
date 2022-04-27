;; Functional implementation of vectors and operations in linear vector space.
;; Vector renamed 2d-vector to avoid namespace clash with vector function.

;; (hypotenuse number number) -> number
;; Computes the longest side of a right triangle.
;; (hypotenuse 3 4) -> 5

(define (hypotenuse x y)
  (sqrt (+ (sqr x) (sqr y))))

;; (vec2D number number) -> pair
;; Returns two-dimensional coordinates as a number pair.
;; (vec2D 3 4) -> '(3 . 4)

(define (vec2D x y)
  (cons x y))

;; I-HAT, J-HAT
;; Mutually orthogonal unit vectors, forming the standard basis.

(define I-HAT (vec2D 1 0))
(define J-HAT (vec2D 0 1))

;; (add vec2D ...) -> vec2D
;; Computes the sum of a series of vectors.
;; (add (vec2D 1 2) (vec2D 1 2)) -> '(2 . 4)

(define (add . vecs)
  (if (= (length vecs) 1)
      (car vecs)
      (foldl (lambda (v1 v2)
               (let ([x1 (car v1)]
                     [y1 (cdr v1)]
                     [x2 (car v2)]
                     [y2 (cdr v2)])
                 (cons (+ x1 x2) (+ y1 y2))))
             '(0 . 0)
             vecs)))

;; (scale vec2D number) -> vec2D
;; Returns a vector multiplied by a number.
;; (scale (vec2D 1 2) 2) -> '(2 . 4)

(define (scale vec factor)
  (let ([x (car vec)]
        [y (cdr vec)])
    (cons (* factor x) (* factor y))))

;; (dot vec2D vec2D) -> number
;; Computes the dot product of two two-dimensional vectors.
;; (dot (vec2D 1 2) (vec2D 3 4)) -> 11

(define (dot v1 v2)
  (let ([x1 (car v1)]
        [y1 (cdr v1)]
        [x2 (car v2)]
        [y2 (cdr v2)])
    (+ (* x1 x2) (* y1 y2))))

;; (cross vec2D vec2D) -> number
;; Computes the cross product of two two-dimensional vectors.
;; (cross (vec2D 1 2) (vec2D 3 4)) -> -2

(define (cross v1 v2)
  (let ([x1 (car v1)]
        [y1 (cdr v1)]
        [x2 (car v2)]
        [y2 (cdr v2)])
    (- (* x1 y2) (* y1 x2))))

;; (magnitude vec2D) -> number
;; Returns the magnitude of a two-dimensional vector.
;; (magnitude (vec2D 3 4)) -> 5

(define (magnitude vec)
  (let ([x (car vec)]
        [y (cdr vec)])
    (hypotenuse x y)))

;; (compare operator) -> (function vec2D...) -> boolean
;; Generates functions for sequentially comparing the magnitudes of a list of two-dimensional vectors.
;; (define vec-gt? (compare >)) -> (vec-gt? (vec2D 3 4) (vec2D 1 2)) -> #t

(define (compare operator)
  (lambda vecs
    (if (< (length vecs) 2)
        #t
        (apply operator
               (map magnitude vecs)))))

;; 2d-vector comparison functions

(define vec-gt? (compare >))
(define vec-lt? (compare <))
(define vec-eq? (compare =))

;; (approximate function) -> (function vec2D) -> vec2D
;; Generates approximation functions for simplifying vector components.
;; (define vec-round (approximate round)) -> (vec-round (vec2D 1.3 1.7)) -> '(1.0 . 2.0)

(define (approximate operation)
  (lambda (vec)
    (let ([x (car vec)]
          [y (cdr vec)])
      (vec2D (operation x) (operation y)))))

(define vec-round (approximate round))
(define vec-ceiling (approximate ceiling))
(define vec-floor (approximate floor))
