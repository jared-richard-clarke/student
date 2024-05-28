;; (divisors 10) -> '(5 2)
;; do syntax: (do ((var init update) ...) (test result ...) expr ...)
;; simplified macro expansion

(define divisors
  (lambda (n)
    (do ([i 2 (+ i 1)]
         [lst '() (if (integer? (/ n i))
                      (cons i lst)
                      lst)])
      ((>= i n) lst))))

; === letrec expansion ===
(define divisors
  (lambda (n)
    (letrec ([loop (lambda (i lst)
                     (if (>= i n)
                         lst                          ;; (begin (if #f #f #f) lst)
                         (loop (+ i 1)                ;; (begin (loop (+ i 1)
                               (if (integer? (/ n i)) ;;              (if (integer? (/ n i))
                                   (cons i lst)       ;;                  (cons i lst)
                                   lst))))])          ;;                  lst)))))])
      (loop 2 (quote ())))))

; === let expansion ===
(define divisors
  (lambda (n)
    (let ((loop #f))
      (let ([loop1 (lambda (i lst)
                    (if (>= i n)
                        lst                           ;; (begin (if #f #f #f) lst)
                        (loop (+ i 1)                 ;; (begin (loop (+ i 1)
                              (if (integer? (/ n i))  ;;              (if (integer? (/ n i))
                                  (cons i lst)        ;;                  (cons i lst)
                                  lst))))])           ;;                  lst)))))])
        (set! loop loop1)
        (let () (loop 2 (quote ())))))))

; === lambda expansion ===
(define divisors
  (lambda (n)
    ((lambda (loop)
       ((lambda (loop1)
          (set! loop loop1)
          ((lambda () (loop 2 (quote ())))))
        (lambda (i lst)
          (if (>= i n)
              lst                                     ;; (begin (if #f #f #f) lst)
              (loop (+ i 1)                           ;; (begin (loop (+ i 1)
                    (if (integer? (/ n i))            ;;              (if (integer? (/ n i))
                        (cons i lst)                  ;;                  (cons i lst)
                        lst))))))                     ;;                  lst)))))])
     #f)))

