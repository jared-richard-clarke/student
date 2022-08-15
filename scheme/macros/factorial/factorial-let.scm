;; factorial: where n! is the product of all positive integers less than or equal to n.
;; ( iterative implementation )
;; example: (factorial 4) -> 24

(define (factorial n)
  (let loop ([product 1]
             [operand n])
    (if (< operand 1)
        product
        (loop (* product operand) 
              (- operand 1)))))

; === letrec expansion ===
(define factorial-letrec
  (lambda (n)
    ((letrec ([loop (lambda (product operand)
                      (if (< operand 1)
                          product
                          (loop (* product operand)
                                (- operand 1))))])
       loop)
     1 n)))

; === let expansion ===
(define factorial-let
  (lambda (n)
    ((let ([loop #f])
       (let ([temp (lambda (product operand)
                     (if (< operand 1)
                         product
                         (loop (* product operand)
                               (- operand 1))))])
         (set! loop temp)
         loop))
     1 n)))

; === lambda expansion ===
(define factorial-lambda
  (lambda (n)
    (((lambda (loop)
        ((lambda (temp)
           (set! loop temp)
           loop)
         (lambda (product operand)
           (if (< operand 1)
               product
               (loop (* product operand)
                     (- operand 1))))))
      #f)
     1 n)))
