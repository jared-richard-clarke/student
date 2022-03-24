(define (vector-object x y)
  ; === instance ===
  (define tip (cons x y))
  ; === methods ===
  (define type 'vector-object)
  
  (define (magnitude)
    (let ([x (car tip)]
          [y (cdr tip)])
      (sqrt (+ (sqr x) (sqr y)))))
  
  (define (add vect)
    (let ([x1 (car tip)]
          [y1 (cdr tip)]
          [x2 (car (vect 'tip))]
          [y2 (cdr (vect 'tip))])
      (set! tip (cons (+ x1 x2) (+ y1 y2)))))
  
  (define (scale factor)
    (set! tip (cons (* factor (car tip))
                    (* factor (cdr tip)))))
  
  ; === interface ===
  (lambda (message . arguments)
    (cond [(eq? message 'tip) tip]
          [(eq? message 'type) type]
          [(eq? message 'magnitude) (magnitude)]
          [(eq? message 'add) (add (car arguments))]
          [(eq? message 'scale) (scale (car arguments))]
          [else (error "invalid input" message)])))
