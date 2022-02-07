(define (factorial n)
  (define (iter result number)
    (if (< number 1)
        result
        (iter (* result number)
              (- number 1))))
  (iter 1 n))

; === letrec* expansion ===
(define factorial-letrec*
  (lambda (n)
    (letrec* ([iter (lambda (result number)
                      (if (< number 1)
                          result
                          (iter (* result number)
                                (- number 1))))])
             (iter 1 n))))

; === let expansion ===
(define factorial-let
  (lambda (n)
    (let ([iter #f])
      (set! iter (lambda (result number)
                   (if (< number 1)
                       result
                       (iter (* result number)
                             (- number 1)))))
      (let ()
        (iter 1 n)))))

; === lambda expansion ===
(define factorial-lambda
  (lambda (n)
    ((lambda (iter)
       (set! iter (lambda (result number)
                    (if (< number 1)
                        result
                        (iter (* result number)
                              (- number 1)))))
       ((lambda ()
          (iter 1 n))))
     #f)))
