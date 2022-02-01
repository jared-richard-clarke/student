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
    ((letrec [(loop (lambda (result count)
                      (if (>= count exponent)
                          result
                          (loop (* result base)
                                (+ 1 count)))))]
       loop)
     1 0)))

; === let expansion ===
(define power-let
  (lambda (base exponent)
    ((let [(loop (void))]
       (let [(newtemp (lambda (result count)
                        (if (>= count exponent)
                            result
                            (loop (* result base)
                                  (+ 1 count)))))]
         (set! loop newtemp)
         loop))
     1 0)))

; === lambda expansion ===
(define power-lambda
  (lambda (base exponent)
    (((lambda (loop)
        ((lambda (newtemp)
           (set! loop newtemp)
           loop)
         (lambda (result count)
           (if (>= count exponent)
               result
               (loop (* result base)
                     (+ 1 count))))))
      (void))
     1 0)))
