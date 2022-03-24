;; Functional implementation of vectors and operations in linear vector space.
;; Vector renamed vect to avoid namespace clash with vector function.

;; (vect number number) -> pair
;; Returns two-dimensional coordinates as a number pair.
;; (vect 3 4) -> '(3 . 4)

(define (vect x y)
  (cons x y))

;; (add vect vect) -> vect
;; Returns the sum of two vectors.
;; (add (vect 1 2) (vect 1 2)) -> '(2 . 4)

(define (add vect-1 vect-2)
  (let ([x1 (car vect-1)]
        [y1 (cdr vect-1)]
        [x2 (car vect-2)]
        [y2 (cdr vect-2)])
    (cons (+ x1 x2) (+ y1 y2))))

;; (scale vect number) -> vect
;; Returns a vector multiplied by a number.
;; (scale (vect 1 2) 2) -> '(2 . 4)

(define (scale vect factor)
  (let ([x (car vect)]
        [y (cdr vect)])
    (cons (* factor x) (* factor y))))

;; (magnitude vect) -> number
;; Returns the magnitude of a vector.
;; (magnitude (vect 3 4)) -> 5

(define (magnitude vect)
  (let ([x (car vect)]
        [y (cdr vect)])
    (sqrt (+ (sqr x) (sqr y)))))
