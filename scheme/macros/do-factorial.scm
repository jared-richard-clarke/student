;; === Macro as defined in R6RS ===
(define-syntax do
  (lambda (x)
    (syntax-case x ()
      [(_ (binding ...) (test res ...) expr ...)
       (with-syntax ([((var val update) ...)
                      (map (lambda (b)
                             (syntax-case b ()
                               [(var val) (syntax (var val var))]
                               [(var val update) (syntax (var val update))]))
                           (syntax (binding ...)))])
         (syntax (let loop ([var val] ...)
                   (if test
                       (begin (if #f #f) res ...)
                       (begin expr ... (loop update ...))))))])))

;; === do expression ===
(define factorial
  (lambda (n)
    (do ([operand n (- operand 1)]
         [product 1 (* product operand)])
      ((zero? operand) product))))

;; === named let expansion ===
(define factorial
  (lambda (n)
    (let loop ([operand n]
               [product 1])
      (if (zero? operand)
          (begin (if #f #f) product)
          (begin (loop (- operand 1) (* product operand)))))))

;; === letrec expansion ===
(define factorial
  (lambda (n)
    ((letrec ([loop (lambda (operand product)
                      (if (zero? operand)
                          (begin (if #f #f) product)
                          (begin (loop (- operand 1) (* product operand)))))])
       loop)
     n
     1)))

;; === let expansion ===
(define factorial
  (lambda (n)
    ((let ([loop #f])
       (let ([loop1 (lambda (operand product)
                      (if (zero? operand)
                          (begin (if #f #f) product)
                          (begin (loop (- operand 1) (* product operand)))))])
         (set! loop loop1)
         (let () loop)))
     n
     1)))

;; === lambda expansion ===
(define factorial
  (lambda (n)
    (((lambda (loop)
        ((lambda (loop1)
           (set! loop loop1)
           ((lambda () loop)))
         (lambda (operand product)
           (if (zero? operand)
               (begin (if #f #f) product)
               (begin (loop (- operand 1) (* product operand)))))))
      #f)
     n
     1)))

;; "The odd-looking expression '(if #f #f)' is inserted before the result expressions 'res ...'
;;  in case no result expressions are provided, since begin requires at least one subexpression.
;;  The value of '(if #f #f)' is unspecified, which is what we want since the value of do is
;;  unspecified if no result expressions are provided."
;;
;; — The Scheme Programming Language, 4th edition
