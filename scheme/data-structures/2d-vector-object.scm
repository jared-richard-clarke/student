;; Object implementation of vectors and operations in linear vector space.

;; (2d-vector number number) -> 2d-vector
;; Creates 2d-vector object implemented as a function. Captures values within its closure.
;; (define v (2d-vector 3 4))
;; (v 'point) -> '(3 . 4)
;; (v 'type) -> '2d-vector
;; (v 'magnitude) -> 5
;; (v 'unit-vector) -> '(3/5 . 4/5)

(define (2d-vector x y)
  ;; === properties ===
  (define point (cons x y))
  
  (define px x)
  (define py y)
  
  (define type '2d-vector)
  
  (define magnitude (sqrt (+ (sqr px) (sqr py))))
  
  (define unit-vector (cons (/ px magnitude) (/ py magnitude)))
  
  ;; === interface ===
  (lambda (message)
    (cond [(eq? message 'point) point]
          [(eq? message 'type) type]
          [(eq? message 'magnitude) magnitude]
          [(eq? message 'unit-vector) unit-vector]
          [else (error "invalid input" message)])))

;; (add 2d-vector 2d-vector) -> 2d-vector
;; Returns a 2d-vector that is the sum of a series of 2d-vectors.
;; (add (2d-vector 1 2) (2d-vector 3 4) (2d-vector 2 1)) -> (2d-vector 'point) -> '(6 . 7)

(define (add . vs)
  (if (= (length vs) 1)
      (car vs)
      ;; To prevent memory consumption, add processes 2d-vectors as a series of pairs,
      ;; converting only the sum to a 2d-vector function with closure. 
      (let ([sum (foldl (lambda (v1 v2)
                          (let ([x1 (car v1)]
                                [y1 (cdr v1)]
                                [x2 (car v2)]
                                [y2 (cdr v2)])
                            (cons (+ x1 x2) (+ y1 y2))))
                        '(0 . 0)
                        (map (lambda (v) (v 'point)) vs))])
        (2d-vector (car sum) (cdr sum)))))

;; (scale 2d-vector number) -> 2d-vector
;; Returns a scales 2d-vector that is the product of a 2d-vector and a number.
;; (scale (2d-vector 3 4) 2) -> (2d-vector 'point) -> '(6 . 8)

(define (scale v factor)
  (let* ([p (v 'point)]
         [x (car p)]
         [y (cdr p)])
    (2d-vector (* x factor) (* y factor))))

;; (dot-product 2d-vector 2d-vector) -> number
;; Computes the dot product of two 2d-vectors.
;; (dot-product (2d-vector 1 2) (2d-vector 3 4)) -> 11

(define (dot-product v1 v2)
  (let* ([p1 (v1 'point)]
         [p2 (v2 'point)]
         [x1 (car p1)]
         [y1 (cdr p1)]
         [x2 (car p2)]
         [y2 (cdr p2)])
    (+ (* x1 x2) (* y1 y2))))

;; (cross-product 2d-vector 2d-vector) -> number
;; Computes the cross product of two 2d-vectors.
;; (cross-product (2d-vector 1 2) (2d-vector 3 4)) -> -2

(define (cross-product v1 v2)
  (let* ([p1 (v1 'point)]
         [p2 (v2 'point)]
         [x1 (car p1)]
         [y1 (cdr p1)]
         [x2 (car p2)]
         [y2 (cdr p2)])
    (- (* x1 y2) (* y1 x2))))

;; (create-comparison operator) -> function
;; Generates functions for sequentially comparing the magnitudes of a list of 2d-vectors.
;; (define vect-gt? (create-comparison >)) -> (vect-gt? (2d-vector 3 4) (2d-vector 1 2)) -> #t

(define (create-comparison operator)
  (lambda vectors
    (if (= (length vectors) 1)
        #t
        (apply operator
               (map (lambda (v) (v 'magnitude)) vectors)))))

;; 2d-vector comparison functions

(define vect-gt? (create-comparison >))
(define vect-lt? (create-comparison <))
(define vect-eq? (create-comparison =))
