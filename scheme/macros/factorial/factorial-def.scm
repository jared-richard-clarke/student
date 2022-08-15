(define (factorial n)
  (define (iter product operand)
    (if (< operand 1)
        product
        (iter (* product operand)
              (- operand 1))))
  (iter 1 n))

; === letrec* expansion ===
(define factorial-letrec*
  (lambda (n)
    (letrec* ([iter (lambda (product operand)
                      (if (< operand 1)
                          product
                          (iter (* product operand)
                                (- operand 1))))])
             (iter 1 n))))

; === let expansion ===
(define factorial-let
  (lambda (n)
    (let ([iter #f])
      (set! iter (lambda (product operand)
                   (if (< operand 1)
                       product
                       (iter (* product operand)
                             (- operand 1)))))
      (let ()
        (iter 1 n)))))

; === lambda expansion ===
(define factorial-lambda
  (lambda (n)
    ((lambda (iter)
       (set! iter (lambda (product operand)
                    (if (< operand 1)
                        product
                        (iter (* product operand)
                              (- operand 1)))))
       ((lambda ()
          (iter 1 n))))
     #f)))
