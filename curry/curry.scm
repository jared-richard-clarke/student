; Scheme: R5RS
(define add
  (lambda (x)
    (lambda (y)
      (+ x y))))

(define add-1 (add 1))
(define seven (add-1 6))

(define eleven ((add 1) 10))
