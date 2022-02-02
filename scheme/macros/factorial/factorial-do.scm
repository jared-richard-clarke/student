; do syntax: (do ((var init update) ...) (test result ...) expr ...)
(define factorial
  (lambda (n)
    (do ([i n (- i 1)] [a 1 (* a i)])
      ((zero? i) a))))

; === letrec expansion ===
(define factorial-letrec
  (lambda (n)
    (letrec ([loop (lambda (i a)
                     (if (zero? i)
                         (begin (if #f #f #f) a)
                         (begin (loop (do "step" i (- i 1))
                                      (do "step" a (* a i))))))])
      (loop n 1))))

; === let expansion ===
(define factorial-let
  (lambda (n)
    (let ([loop #f])
      (let ([temp (lambda (i a)
                    (if (zero? i)
                        (begin (if #f #f #f) a)
                        (begin (loop (do "step" i (- i 1))
                                     (do "step" a (* a i))))))])
        (set! loop temp)
        (loop n 1)))))

; === lambda expansion ===
(define factorial-lambda
  (lambda (n)
    ((lambda (loop)
       ((lambda (temp)
          (set! loop temp)
          (loop n 1))
        (lambda (i a)
          (if (zero? i)
              (begin (if #f #f #f) a)
              (begin (loop (- i 1)
                           (* a i)))))))
     #f)))
