; (range 1 10) -> '(1 2 3 4 5 6 7 8 9 10)

; === base ===
(define (range start stop)
  (let loop ([count stop]
             [result '()])
    (if (> start count)
        result
        (loop (- count 1)
              (cons count result)))))

; === letrec expansion ===
(define range-letrec
  (lambda (start stop)
    ((letrec ([loop (lambda (count result)
                      (if (> start count)
                          result
                          (loop (- count 1)
                                (cons count result))))])
       loop)
     stop '())))

; === let expansion ===
(define range-let
  (lambda (start stop)
    ((let ([loop #f])
       (let ([temp (lambda (count result)
                     (if (> start count)
                         result
                         (loop (- count 1)
                               (cons count result))))])
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
         (lambda (count result)
           (if (> start count)
               result
               (loop (- count 1)
                     (cons count result))))))
      #f)
     stop '())))
