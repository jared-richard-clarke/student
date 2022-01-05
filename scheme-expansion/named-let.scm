; named let
(let sum ([x 5])
  (if (zero? x)
      0
      (+ x (sum (- x 1)))))

; expanded to letrec
((letrec ([sum (lambda (x)
                 (if (zero? x)
                     0
                     (+ x (sum (- x 1)))))])
   sum)
 5)

; expanded to nested let expressions
(let ([x 5])
  (let ([sum #f])
    (let ([temp (lambda (x)
                  (if (zero? x)
                      0
                      (+ x (sum (- x 1)))))])
      (set! sum temp)
      (let ()
        (sum x)))))

; expanded to lambda expressions
((lambda (x)
   ((lambda (sum)
      ((lambda (temp)
         (set! sum temp)
         ((lambda ()
            (sum x))))
       (lambda (x)
         (if (zero? x)
             0
             (+ x (sum (- x 1)))))))
    #f))
 5)
