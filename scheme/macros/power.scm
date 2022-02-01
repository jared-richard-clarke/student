; (power 2 3) -> 8

; === base ===
(define (power base exponent)
  (let loop ([result 1]
             [count 0])
    (if (>= count exponent)
        result
        (loop (* result base)
              (+ 1 count)))))

; === letrec expansion ===
(define power-letrec
  (lambda (base exponent)
    ((letrec ([loop (lambda (result count)
                      (if (>= count exponent)
                          result
                          (loop (* result base)
                                (+ 1 count))))])
       loop)
     1 0)))

; === let expansion ===
(define power-let
  (lambda (base exponent)
    ((let ([loop #f])
       (let ([temp (lambda (result count)
                        (if (>= count exponent)
                            result
                            (loop (* result base)
                                  (+ 1 count))))])
         (set! loop temp)
         loop))
     1 0)))

; === lambda expansion ===
(define power-lambda
  (lambda (base exponent)
    (((lambda (loop)
        ((lambda (temp)
           (set! loop temp)
           loop)
         (lambda (result count)
           (if (>= count exponent)
               result
               (loop (* result base)
                     (+ 1 count))))))
      #f)
     1 0)))
