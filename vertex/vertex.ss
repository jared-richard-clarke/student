;; Scheme: R5RS
(define (vertex x y)
  (define (magnitude)
    (sqrt (+ (* x x) (* y y))))
  (lambda (message)
    (cond
      ((equal? 'x message) x)
      ((equal? 'y message) y)
      ((equal? 'magnitude message) (magnitude)))))

(define v1 (vertex 3 4))
(define v2 (vertex 1 2))

(v1 'magnitude) ; -> 5
(v2 'x)      ; -> 1
