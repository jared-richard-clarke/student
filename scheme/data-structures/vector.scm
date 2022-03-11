(define (vector-object x y)
  ; === instance ===
  (define point (cons x y))
  ; === methods ===
  (define type 'vector)
  
  (define (magnitude)
    (let ([x (car point)]
          [y (cdr point)])
      (sqrt (+ (sqr x) (sqr y)))))

  (define (scale factor)
    (set! point (cons (* factor (car point))
                      (* factor (cdr point)))))
  
  ; === interface ===
  (lambda (message . arguments)
    (cond [(eq? message 'point) point]
          [(eq? message 'type) type]
          [(eq? message 'magnitude) (magnitude)]
          [(eq? message 'scale) (scale (car arguments))]
          [else (error "invalid input" message)])))
