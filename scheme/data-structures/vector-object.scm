;; Object implementation of vectors and operations in linear vector space.
;; Vector renamed vector-object to avoid namespace clash with vector function.

(define (vector-object x y)
  ;; === instance ===
  ;; Contains vector object state.
  ;; Call to receive instance of state.
  (define tip (cons x y))
  ;; === methods ===
  ;; Returns type of vector object.
  (define type 'vector-object)
  ;; Returns the magnitude of a vector object.
  (define (magnitude)
    (let ([x (car tip)]
          [y (cdr tip)])
      (sqrt (+ (sqr x) (sqr y)))))
  ;; Adds vector object to vector object. Mutates object. Returns void.
  (define (add vect)
    (let ([x1 (car tip)]
          [y1 (cdr tip)]
          [x2 (car (vect 'tip))]
          [y2 (cdr (vect 'tip))])
      (set! tip (cons (+ x1 x2) (+ y1 y2)))))
  ;; Scales vector object by factor. Mutates object. Returns void.
  (define (scale factor)
    (set! tip (cons (* factor (car tip))
                    (* factor (cdr tip)))))
  
  ;; === interface ===
  ;; Send specific messages to object to receive create specific behaviors
  (lambda (message . arguments)
    (cond [(eq? message 'tip) tip]
          [(eq? message 'type) type]
          [(eq? message 'magnitude) (magnitude)]
          [(eq? message 'add) (add (car arguments))]
          [(eq? message 'scale) (scale (car arguments))]
          [else (error "invalid input" message)])))
