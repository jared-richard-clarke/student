;; Functional implementation of vectors and operations in linear vector space.
;; Vector renamed 2d-vector to avoid namespace clash with vector function.

;; (2d-vector number number) -> pair
;; Returns two-dimensional coordinates as a number pair.
;; (2d-vector 3 4) -> '(3 . 4)

(define (2d-vector x y)
  (cons x y))

;; (add 2d-vector 2d-vector) -> 2d-vector
;; Returns the sum of two vectors.
;; (add (2d-vector 1 2) (2d-vector 1 2)) -> '(2 . 4)

(define (add v1 v2)
  (let ([x1 (car v1)]
        [y1 (cdr v1)]
        [x2 (car v2)]
        [y2 (cdr v2)])
    (cons (+ x1 x2) (+ y1 y2))))

;; (scale 2d-vector number) -> vect
;; Returns a vector multiplied by a number.
;; (scale (2d-vector 1 2) 2) -> '(2 . 4)

(define (scale v factor)
  (let ([x (car v)]
        [y (cdr v)])
    (cons (* factor x) (* factor y))))

;; (magn 2d-vector) -> number
;; Returns the magnitude of a 2d vector.
;; (magn (2d-vector 3 4)) -> 5

(define (magn v)
  (let ([x (car v)]
        [y (cdr v)])
    (sqrt (+ (sqr x) (sqr y)))))

;; (memoize function) -> function -> value
;; Wraps function in a function that stores previously-computed values.
;; Eliminates redundant computation.
;; (define add (memoize (lambda (x y) (+ x y))))
;; (add 1 6) -> caches then returns 7

(define (memoize function)
  (define cache '())
  (lambda args
    (let ([cached (assoc args cache)])
      (if (not cached)
          (let ([result (apply function args)])
            (set! cache (cons (cons args result) cache))
            result)
          (cdr cached)))))

;; magnitude is magn except that it caches previously-computed values.

(define magnitude (memoize magn))
