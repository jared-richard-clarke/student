; do syntax: (do ((var init update) ...) (test result ...) expr ...)
; simplified macro expansion for clarity.

(define (factorial n)
  (do ([i n (- i 1)] [a 1 (* a i)])
    ((zero? i) a)))

; === letrec expansion ===
(define factorial-letrec
  (lambda (n)
    (letrec ([loop (lambda (i a)
                     (if (zero? i)
                         a
                         (loop (- i 1)
                               (* a i))))])
      (loop n 1))))

; === let expansion ===
(define factorial-let
  (lambda (n)
    (let ([loop #f])
      (let ([temp (lambda (i a)
                    (if (zero? i)
                        a
                        (loop (- i 1)
                              (* a i))))])
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
              a
              (loop (- i 1)
                    (* a i))))))
     #f)))
