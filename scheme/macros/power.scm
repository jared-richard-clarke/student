;; (power 2 3) -> 8

;; === base ===
(define (power base exponent)
  (let loop ([product base]
             [iter 1])
    (if (>= iter exponent)
        product
        (loop (* product base)
              (+ iter 1)))))

;; === letrec expansion ===
(define power-letrec
  (lambda (base exponent)
    ((letrec ([loop (lambda (product iter)
                      (if (>= iter exponent)
                          product
                          (loop (* product base)
                                (+ iter 1))))])
       loop)
     base 1)))

;; === let expansion ===
(define power-let
  (lambda (base exponent)
    ((let ([loop #f])
       (let ([temp (lambda (product iter)
                     (if (>= iter exponent)
                         product
                         (loop (* product base)
                               (+ iter 1))))])
         (set! loop temp)
         loop))
     base 1)))

;; === lambda expansion ===
(define power-lambda
  (lambda (base exponent)
    (((lambda (loop)
        ((lambda (temp)
           (set! loop temp)
           loop)
         (lambda (product iter)
           (if (>= iter exponent)
               product
               (loop (* product base)
                     (+ iter 1))))))
      #f)
     base 1)))
