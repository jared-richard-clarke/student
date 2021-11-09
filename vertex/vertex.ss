;; Scheme: R5RS
(define (vertex x y)
  (define (origin)
    (sqrt (+ (* x x) (* y y))))
  (lambda (message)
    (cond
      ((equal? 'x message) x)
      ((equal? 'y message) y)
      ((equal? 'origin message) (origin)))))

(define v1 (vertex 3 4))
(define v2 (vertex 1 2))

(v1 'origin) ; -> 5
(v2 'x)      ; -> 1
