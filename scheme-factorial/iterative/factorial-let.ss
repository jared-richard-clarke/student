(define factorial
  (lambda (x)
    (let ([iter "init"])
      (let ([temp (lambda (result counter)
                    (if (> counter x)
                        result
                        (iter (* result counter) (+ counter 1))))])
        (set! iter temp)
        (let ()
          (iter 1 1))))))
(factorial 4)
