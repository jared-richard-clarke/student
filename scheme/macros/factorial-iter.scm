; base
(define (factorial n)
  (let loop ([result 1]
             [number n])
    (if (< number 1)
        result
        (loop (* result number) (- number 1)))))

; letrec expansion
(define factorial-letr
  (lambda (n)
    ((letrec ((loop
               (lambda (result number)
                 (if (< number 1) result (loop (* result number) (- number 1))))))
       loop)
     1
     n)))

; lambda expansion
(define factorial-lambda
  (lambda (n)
    (((lambda (loop)
        (set! loop
              (lambda (result number)
                (if (< number 1) result (loop (* result number) (- number 1)))))
        ((lambda () loop)))
      #f)
     1
     n)))
