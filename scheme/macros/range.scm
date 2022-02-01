; (range 1 10) -> '(1 2 3 4 5 6 7 8 9 10)

; === base ===
(define (range start stop)
  (let loop ([number stop]
             [result '()])
    (if (< number start)
        result
        (loop (- number 1)
              (cons number result)))))

; === letrec expansion ===
(define range-letrec
  (lambda (start stop)
    ((letrec ([loop (lambda (number result)
                      (if (< number start)
                          result
                          (loop (- number 1)
                                (cons number result))))])
       loop)
     stop '())))

; === let expansion ===
(define range-let
  (lambda (start stop)
    ((let ([loop #f])
       (let ([temp (lambda (number result)
                     (if (< number start)
                         result
                         (loop (- number 1)
                               (cons number result))))])
         (set! loop temp)
         loop))
     stop '())))

; === lambda expansion ===
(define range-lambda
  (lambda (start stop)
    (((lambda (loop)
        ((lambda (temp)
           (set! loop temp)
           loop)
         (lambda (number result)
           (if (< number start)
               result
               (loop (- number 1)
                     (cons number result))))))
      #f)
     stop '())))
