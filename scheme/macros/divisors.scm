; (divisors 10) -> '(5 2)
; do syntax: (do ((var init update) ...) (test result ...) expr ...)
; simplified macro expansion

(define divisors
  (lambda (n)
    (do ([i 2 (+ i 1)]
         [lst '()
              (if (integer? (/ n i))
                  (cons i lst)
                  lst)])
      ((>= i n) lst))))

; === letrec expansion ===
(define divisors-letrec
  (lambda (n)
    (letrec ([loop (lambda (i lst)
                     (if (>= i n)
                         lst                          ; (begin (if #f #f #f) lst)
                         (loop (+ i 1)                ; (begin (loop (+ i 1)
                               (if (integer? (/ n i)) ;              (if (integer? (/ n i))
                                   (cons i lst)       ;                  (cons i lst)
                                   lst))))])          ;                  lst)))))])
      (loop 2 (quote ())))))

; === let expansion ===
(define divisors-let
  (lambda (n)
    (let ((loop #f))
      (let ([temp (lambda (i lst)
                    (if (>= i n)
                        lst                           ; (begin (if #f #f #f) lst)
                        (loop (+ i 1)                 ; (begin (loop (+ i 1)
                              (if (integer? (/ n i))  ;              (if (integer? (/ n i))
                                  (cons i lst)        ;                  (cons i lst)
                                  lst))))])           ;                  lst)))))])
        (set! loop temp)
        (loop 2 (quote ()))))))

; === lambda expansion ===
(define divisors-lambda
  (lambda (n)
    ((lambda (loop)
       ((lambda (temp)
          (set! loop temp)
          (loop 2 (quote ())))
        (lambda (i lst)
          (if (>= i n)
              lst                                     ; (begin (if #f #f #f) lst)
              (loop (+ i 1)                           ; (begin (loop (+ i 1)
                    (if (integer? (/ n i))            ;              (if (integer? (/ n i))
                        (cons i lst)                  ;                  (cons i lst)
                        lst))))))                     ;                  lst)))))])
     #f)))
