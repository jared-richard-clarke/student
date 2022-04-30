;; Functional implementation of vectors and operations in linear vector space.

;; (hypotenuse number number) -> number
;; Computes the longest side of a right triangle.
;; (hypotenuse 3 4) -> 5

(define (hypotenuse x y)
  (sqrt (+ (sqr x) (sqr y))))

;; (vec2 number number) -> pair
;; Returns two-dimensional coordinates as a number pair.
;; (vec2 3 4) -> '(3 . 4)

(define (vec2 x y)
  (cons x y))

;; I-HAT, J-HAT
;; Mutually orthogonal unit vectors, forming the standard basis.

(define I-HAT (vec2 1 0))
(define J-HAT (vec2 0 1))

;; (add vec2 ...) -> vec2
;; Computes the sum of a series of vectors.
;; (add (vec2 1 2) (vec2 1 2)) -> '(2 . 4)

(define (add . vecs)
  (if (= (length vecs) 1)
      (car vecs)
      (foldl (lambda (v1 v2)
               (let ([x1 (car v1)]
                     [y1 (cdr v1)]
                     [x2 (car v2)]
                     [y2 (cdr v2)])
                 (vec2 (+ x1 x2) (+ y1 y2))))
             '(0 . 0)
             vecs)))

;; (scale vec2 number) -> vec2
;; Returns a vector multiplied by a number.
;; (scale (vec2 1 2) 2) -> '(2 . 4)

(define (scale vec factor)
  (let ([x (car vec)]
        [y (cdr vec)])
    (cons (* factor x) (* factor y))))

;; (dot vec2 vec2) -> number
;; Computes the dot product of two two-dimensional vectors.
;; (dot (vec2 1 2) (vec2 3 4)) -> 11

(define (dot v1 v2)
  (let ([x1 (car v1)]
        [y1 (cdr v1)]
        [x2 (car v2)]
        [y2 (cdr v2)])
    (+ (* x1 x2) (* y1 y2))))

;; (cross vec2 vec2) -> number
;; Computes the cross product of two two-dimensional vectors.
;; (cross (vec2 1 2) (vec2 3 4)) -> -2

(define (cross v1 v2)
  (let ([x1 (car v1)]
        [y1 (cdr v1)]
        [x2 (car v2)]
        [y2 (cdr v2)])
    (- (* x1 y2) (* y1 x2))))

;; (magnitude vec2) -> number
;; Returns the magnitude of a two-dimensional vector.
;; (magnitude (vec2 3 4)) -> 5

(define (magnitude vec)
  (let ([x (car vec)]
        [y (cdr vec)])
    (hypotenuse x y)))

;; (compare operator) -> (function vec2...) -> boolean
;; Generates functions for sequentially comparing the magnitudes of a list of two-dimensional vectors.
;; (define vec-gt? (compare >)) -> (vec-gt? (vec2 3 4) (vec2 1 2)) -> #t

(define (compare operator)
  (lambda vecs
    (if (< (length vecs) 2)
        #t
        (apply operator
               (map magnitude vecs)))))

;; vec2 comparison functions

(define vec-gt? (compare >))
(define vec-lt? (compare <))
(define vec-eq? (compare =))

;; (approximate function) -> (function vec2) -> vec2
;; Generates approximation functions for simplifying vector components.
;; (define vec-round (approximate round)) -> (vec-round (vec2 1.3 1.7)) -> '(1.0 . 2.0)

(define (approximate operation)
  (lambda (vec)
    (let ([x (car vec)]
          [y (cdr vec)])
      (vec2 (operation x) (operation y)))))

(define vec-round (approximate round))
(define vec-ceiling (approximate ceiling))
(define vec-floor (approximate floor))

;; === testing ===

;; (assert-equal expression value) -> current-output-port
;; If expression does not evaluate to value, macro prints failed test to current-output-port.
;; (assert-equal (vec2 4 4) '(3 . 4)) ->
;; Test: (vec2 4 4)
;; Expect: (3 . 4), Got: (4 . 4)

(define-syntax assert-equal
  (syntax-rules ()
    [(_ expression value)
     (when (not (equal? expression value))
       (printf "Test: ~a\nExpect: ~a, Got: ~a\n"
               (quote expression)
               value
               expression))]))

;; === unit tests ===

(assert-equal (hypotenuse 3 4) 5)
(assert-equal (vec2 3 4) '(3 . 4))
(assert-equal (scale (vec2 1 2) 2) '(2 . 4))
(assert-equal (cross (vec2 1 2) (vec2 3 4)) -2)
(assert-equal (magnitude (vec2 3 4)) 5)
(assert-equal (vec-gt? (vec2 3 4) (vec2 1 2)) #t)
(assert-equal (vec-lt? (vec2 3 4) (vec2 1 2)) #f)
(assert-equal (vec-eq? (vec2 3 4) (vec2 3 4)) #t)
(assert-equal (vec-round (vec2 1.3 1.7)) '(1.0 . 2.0))
(assert-equal (vec-ceiling (vec2 1.3 1.7)) '(2.0 . 2.0))
(assert-equal (vec-floor (vec2 1.3 1.7)) '(1.0 . 1.0))
