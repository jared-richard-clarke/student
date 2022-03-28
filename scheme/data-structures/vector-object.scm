;; Object implementation of vectors and operations in linear vector space.

;; (2d-vector number number) -> 2d-vector
;; Creates 2d-vector object.
;; (define vector-object (2d-vector 3 4))
;; (vector-object 'point) -> '(3 . 4)
;; (vector-object 'type) -> '2d-vector
;; (vector-object 'magnitude) -> 5

(define (2d-vector x y)
  ;; === properties ===
  (define point (cons x y))
  
  (define type '2d-vector)
  
  (define (magnitude)
    (let ([x (car point)]
          [y (cdr point)])
      (sqrt (+ (sqr x) (sqr y)))))
  
  ;; === interface ===
  (lambda (message)
    (cond [(eq? message 'point) point]
          [(eq? message 'type) type]
          [(eq? message 'magnitude) (magnitude)]
          [else (error "invalid input" message)])))

;; (add 2d-vector 2d-vector) -> 2d-vector
;; Computes and returns a 2d-vector that is the sum of two 2d-vectors.
;; (add (2d-vector 1 2) (2d-vector 3 4)) -> (2d-vector 'point) -> '(4 . 5)

(define (add v1 v2)
  (let* ([p1 (v1 'point)]
         [p2 (v2 'point)]
         [x1 (car p1)]
         [y1 (cdr p1)]
         [x2 (car p2)]
         [y2 (cdr p2)])
    (2d-vector (+ x1 x2) (+ y1 y2))))

;; (scale 2d-vector number) -> 2d-vector
;; Returns a scales 2d-vector that is the product of a 2d-vector and a number.
;; (scale (2d-vector 3 4) 2) -> (2d-vector 'point) -> '(6 . 8)

(define (scale v factor)
  (let* ([p (v 'point)]
         [x (car p)]
         [y (cdr p)])
    (2d-vector (* x factor) (* y factor))))
