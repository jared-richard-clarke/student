;; do syntax: (do ((var init update) ...) (test result ...) expr ...)
;; simplified macro expansion for clarity.

(define (factorial n)
  (do ([operand n (- operand 1)]
       [product 1 (* product operand)])
    ((< operand 1) product)))

;; === letrec expansion ===
(define factorial-letrec
  (lambda (n)
    (letrec ([loop (lambda (operand product)
                     (if (< operand 1)
                         product                        ;; (begin (if #f #f #f) product)
                         (loop (- operand 1)            ;; (begin (loop (do "step" operand (- operand 1))
                               (* product operand))))]) ;;              (do "step" product (* product operand))))))])
      (loop n 1))))

;; === let expansion ===
(define factorial-let
  (lambda (n)
    (let ([loop #f])
      (let ([temp (lambda (operand product)
                    (if (< operand 1)
                        product                         ;; (begin (if #f #f #f) result)
                        (loop (- operand 1)             ;; (begin (loop (do "step" operand (- operand 1))
                              (* product operand))))])  ;;              (do "step" product (* product operand))))))])
        (set! loop temp)
        (loop n 1)))))

;; === lambda expansion ===
(define factorial-lambda
  (lambda (n)
    ((lambda (loop)
       ((lambda (temp)
          (set! loop temp)
          (loop n 1))
        (lambda (operand product)
          (if (< operand 1)
              product
              (loop (- operand 1)
                    (* product operand))))))
     #f)))
