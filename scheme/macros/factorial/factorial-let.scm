; do syntax: (do ((var init update) ...) (test result ...) expr ...)
; simplified macro expansion for clarity.

(define (factorial n)
  (do ([number n (- number 1)] [result 1 (* result number)])
    ((zero? number) result)))

; === letrec expansion ===
(define factorial-letrec
  (lambda (n)
    (letrec ([loop (lambda (number result)
                     (if (zero? number)
                         result
                         (loop (- number 1)
                               (* result number))))])
      (loop n 1))))

; === let expansion ===
(define factorial-let
  (lambda (n)
    (let ([loop #f])
      (let ([temp (lambda (number result)
                    (if (zero? number)
                        result
                        (loop (- number 1)
                              (* result number))))])
        (set! loop temp)
        (loop n 1)))))

; === lambda expansion ===
(define factorial-lambda
  (lambda (n)
    ((lambda (loop)
       ((lambda (temp)
          (set! loop temp)
          (loop n 1))
        (lambda (number result)
          (if (zero? number)
              result
              (loop (- number 1)
                    (* result number))))))
     #f)))
