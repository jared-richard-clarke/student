(define (vector-object x y)
  ; === instance ===
  (define tip (cons x y))
  ; === methods ===
  (define type 'vector-object)
  
  (define (magnitude)
    (let ([x (car tip)]
          [y (cdr tip)])
      (sqrt (+ (sqr x) (sqr y)))))

  (define (scale factor)
    (set! tip (cons (* factor (car tip))
                      (* factor (cdr tip)))))
  
  ; === interface ===
  (lambda (message . arguments)
    (cond [(eq? message 'tip) tip]
          [(eq? message 'type) type]
          [(eq? message 'magnitude) (magnitude)]
          [(eq? message 'scale) (scale (car arguments))]
          [else (error "invalid input" message)])))
