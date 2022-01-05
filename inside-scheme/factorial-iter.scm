; factorial base
(define (factorial x)
  (letrec ([iter (lambda (result counter)
                   (if (> counter x)
                       result
                       (iter (* result counter) (+ counter 1))))])
    (iter 1 1)))

; let expansion
(define factorial-let
  (lambda (x)
    (let ([iter "init"])
      (let ([temp (lambda (result counter)
                    (if (> counter x)
                        result
                        (iter (* result counter) (+ counter 1))))])
        (set! iter temp)
        (let ()
          (iter 1 1))))))

; lambda expansion
(define factorial-lambda
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
