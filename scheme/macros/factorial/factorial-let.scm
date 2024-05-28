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

;; === letrec expansion ===
(define factorial
  (lambda (n)
    ((letrec ((loop
               (lambda (product operand)
                 (if (< operand 1)
                     product
                     (loop (* product operand) (- operand 1))))))
       loop)
     1
     n)))

;; === let expansion ===
(define factorial
  (lambda (n)
    ((let ((loop #f))
       (let ((loop1
              (lambda (product operand)
                (if (< operand 1)
                    product
                    (loop (* product operand) (- operand 1))))))
         (set! loop loop1)
         (let () loop)))
     1
     n)))

;; === lambda expansion ===
(define factorial
  (lambda (n)
    (((lambda (loop)
        ((lambda (loop1)
           (set! loop loop1)
           ((lambda () loop)))
         (lambda (product operand)
           (if (< operand 1)
               product
               (loop (* product operand) (- operand 1))))))
      #f)
     1
     n)))
