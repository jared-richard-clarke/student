(define (vector x y)
  ; === instance ===
  (define point (cons x y))
  ; === methods ===
  (define type 'vector)
  
  (define (magnitude)
    (let ([x (car point)]
          [y (cdr point)])
      (sqrt (+ (sqr x) (sqr y)))))
  
  ; === interface ===
  (lambda (message . arguments)
    (cond [(eq? message 'point) point]
          [(eq? message 'type) type]
          [(eq? message 'magnitude) (magnitude)]
          [else (error "invalid input")])))
