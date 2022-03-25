;; Object implementation of vectors and operations in linear vector space.
;; Vector renamed 2d-vector to avoid namespace clash with vector function.

(define (2d-vector x y)
  ;; === instance ===
  ;; Contains vector object state.
  ;; Call to receive instance of state.
  (define point (cons x y))
  ;; === methods ===
  ;; Returns type of vector object.
  (define type '2d-vector)
  ;; Returns the magnitude of a vector object.
  (define (magnitude)
    (let ([x (car point)]
          [y (cdr point)])
      (sqrt (+ (sqr x) (sqr y)))))
  ;; Adds vector object to vector object. Mutates object. Returns void.
  (define (add v)
    (let ([x1 (car point)]
          [y1 (cdr point)]
          [x2 (car (v 'point))]
          [y2 (cdr (v 'point))])
      (set! point (cons (+ x1 x2) (+ y1 y2)))))
  ;; Scales vector object by factor. Mutates object. Returns void.
  (define (scale factor)
    (set! point (cons (* factor (car point))
                      (* factor (cdr point)))))
  
  ;; === interface ===
  ;; Send specific messages to object to receive create specific behaviors
  (lambda (message . arguments)
    (cond [(eq? message 'point) point]
          [(eq? message 'type) type]
          [(eq? message 'magnitude) (magnitude)]
          [(eq? message 'add) (add (car arguments))]
          [(eq? message 'scale) (scale (car arguments))]
          [else (error "invalid input" message)])))
