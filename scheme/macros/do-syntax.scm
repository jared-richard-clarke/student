;; === Macro as defined in R6RS ===
;; (with-syntax ((pattern syntax) ...) body-1 body-2 ...)
(define-syntax with-syntax
  (lambda (x)
    (syntax-case x ()
      [(_ ((pattern stx) ...) body-1 body-2 ...)
       (syntax (syntax-case (list stx ...) ()
                 [(pattern ...) (let () body-1 body-2 ...)]))])))

;; === Macro as defined in R6RS ===
;; (do ((variable value update) ...) (test result ...) expression ...)
(define-syntax do
  (lambda (x)
    (syntax-case x ()
      [(_ (binding ...) (test result ...) expression ...)
       (with-syntax ([((variable value update) ...) ;; <- with-syntax: pattern
                      (map (lambda (b) ;; <-------------- with-syntax: syntax
                             (syntax-case b ()
                               [(variable value) (syntax (variable value variable))]
                               [(variable value update) (syntax (variable value update))]))
                           (syntax (binding ...)))])
         (syntax (let loop ([variable value] ...) ;; <--- with-syntax: body
                   (if test
                       (begin (if #f #f) result ...)
                       (begin expression ... (loop update ...))))))])))

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
       (let ([loop-1 (lambda (operand product)
                       (if (zero? operand)
                           (begin (if #f #f) product)
                           (begin (loop (- operand 1) (* product operand)))))])
         (set! loop loop-1)
         (let () loop)))
     n
     1)))

;; === lambda expansion ===
(define factorial
  (lambda (n)
    (((lambda (loop)
        ((lambda (loop-1)
           (set! loop loop-1)
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

;; === do expression ===
(define scale-vector!
  (lambda (v k)
    (let ([n (vector-length v)])
      (do ([i 0 (+ i 1)])
        ((= i n))
        (vector-set! v i (* (vector-ref v i) k))))))

;; === named let expansion ===
(define scale-vector!
  (lambda (v k)
    (let ([n (vector-length v)])
      (let loop ((i 0))
        (if (= i n)
            (begin (if #f #f))
            (begin (vector-set! v i (* (vector-ref v i) k))
                   (loop (+ i 1))))))))
