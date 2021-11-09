(define (factorial x)
  (letrec ([iter (lambda (result counter)
                   (if (> counter x)
                       result
                       (iter (* result counter) (+ counter 1))))])
    (iter 1 1)))
(factorial 4)
