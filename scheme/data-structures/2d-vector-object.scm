;; Object implementation of vectors and operations in linear vector space.

;; (vec2D number number) -> vec2D
;; Creates two-dimensional vector object implemented as a function. Captures values within its closure.
;; (define v (vec2D 3 4))
;; (v 'point) -> '(3 . 4)
;; (v 'type) -> 'vec2D
;; (v 'magnitude) -> 5

(define (vec2D x y)
  ;; === properties ===
  (let* ([px x]
         [py y]
         [point (cons x y)]
         [type 'vec2D]
         [magnitude (sqrt (+ (sqr px) (sqr py)))])
    ;; === interface ===
    (lambda (message)
      (cond [(eq? message 'point) point]
            [(eq? message 'type) type]
            [(eq? message 'magnitude) magnitude]
            [else (error "invalid input:" message)]))))

;; I-HAT, J-HAT
;; Mutually orthogonal two-dimensional unit vectors, forming the standard basis.

(define I-HAT (vec2D 1 0))
(define J-HAT (vec2D 0 1))

;; (add vec2D vec2D) -> vec2D
;; Returns a two-dimensional vector that is the sum of a series of two-dimensionsal vectors.
;; (add (vec2D 1 2) (vec2D 3 4) (vec2D 2 1)) -> (vec2D 'point) -> '(6 . 7)

(define (add . vecs)
  (if (= (length vecs) 1)
      (car vecs)
      ;; To prevent memory consumption, add processes vec2Ds as a series of pairs,
      ;; converting only the sum to a vec2D function with closure. 
      (let ([sum (foldl (lambda (v1 v2)
                          (let ([x1 (car v1)]
                                [y1 (cdr v1)]
                                [x2 (car v2)]
                                [y2 (cdr v2)])
                            (cons (+ x1 x2) (+ y1 y2))))
                        '(0 . 0)
                        (map (lambda (v) (v 'point)) vecs))])
        (vec2D (car sum) (cdr sum)))))

;; (scale vec2D number) -> vec2D
;; Returns a scaled two-dimensional vector that is the product of a two-dimensional vector and a number.
;; (scale (vec2D 3 4) 2) -> (vec2D 'point) -> '(6 . 8)

(define (scale vec factor)
  (let* ([p (vec 'point)]
         [x (car p)]
         [y (cdr p)])
    (vec2D (* x factor) (* y factor))))

;; (dot vec2D vec2D) -> number
;; Computes the dot product of two two-dimensional vectors.
;; (dot (vec2D 1 2) (vec2D 3 4)) -> 11

(define (dot v1 v2)
  (let* ([p1 (v1 'point)]
         [p2 (v2 'point)]
         [x1 (car p1)]
         [y1 (cdr p1)]
         [x2 (car p2)]
         [y2 (cdr p2)])
    (+ (* x1 x2) (* y1 y2))))

;; (cross vec2D vec2D) -> number
;; Computes the cross product of two 2d-vectors.
;; (cross (2d-vector 1 2) (2d-vector 3 4)) -> -2

(define (cross v1 v2)
  (let* ([p1 (v1 'point)]
         [p2 (v2 'point)]
         [x1 (car p1)]
         [y1 (cdr p1)]
         [x2 (car p2)]
         [y2 (cdr p2)])
    (- (* x1 y2) (* y1 x2))))

;; (compare operator) -> function
;; Generates functions for sequentially comparing the magnitudes of a list of two-dimensional vectors.
;; (define vec-gt? (compare >)) -> (vec-gt? (vec2D 3 4) (vec2D 1 2)) -> #t

(define (compare operator)
  (lambda vecs
    (if (= (length vecs) 1)
        #t
        (apply operator
               (map (lambda (v) (v 'magnitude)) vecs)))))

;; vec2D comparison functions

(define vec-gt? (compare >))
(define vec-lt? (compare <))
(define vec-eq? (compare =))

;; (approximate function) -> (function vec2D) -> vec2D
;; Generates approximation functions for simplifying vector components.
;; (define vec-round (approximate round)) -> ((vec-round (vec2D 1.3 1.7)) 'point) -> '(1.0 . 2.0)

(define (approximate operation)
  (lambda (vec)
    (let* ([pt (vec 'point)]
           [x (car pt)]
           [y (cdr pt)])
      (vec2D (operation x) (operation y)))))

(define vec-round (approximate round))
(define vec-ceiling (approximate ceiling))
(define vec-floor (approximate floor))
