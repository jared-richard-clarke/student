(define factorial
  (lambda (x)
    ((lambda (iter)
       ((lambda (temp)
          (set! iter temp)
          ((lambda ()
             (iter 1 1))))
        (lambda (result counter)
          (if (> counter x)
              result
              (iter (* result counter) (+ counter 1))))))
     "init")))
(factorial 4)
