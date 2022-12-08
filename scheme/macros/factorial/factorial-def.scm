(define (factorial n)
  (define (loop product operand)
    (if (< operand 1)
        product
        (loop (* product operand)
              (- operand 1))))
  (loop 1 n))

; === letrec* expansion ===
(define factorial-letrec*
  (lambda (n)
    (letrec* ([loop (lambda (product operand)
                      (if (< operand 1)
                          product
                          (loop (* product operand)
                                (- operand 1))))])
             (loop 1 n))))

; === let expansion ===
(define factorial-let
  (lambda (n)
    (let ([loop #f])
      (set! loop (lambda (product operand)
                   (if (< operand 1)
                       product
                       (loop (* product operand)
                             (- operand 1)))))
      (let ()
        (loop 1 n)))))

; === lambda expansion ===
(define factorial-lambda
  (lambda (n)
    ((lambda (loop)
       (set! loop (lambda (product operand)
                    (if (< operand 1)
                        product
                        (loop (* product operand)
                              (- operand 1)))))
       ((lambda ()
          (loop 1 n))))
     #f)))
