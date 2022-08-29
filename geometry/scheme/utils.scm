(library (utils)
         (export hypotenuse approx-eq? assert-equal)
         (import (scheme))

         ;; (hypotenuse number number) -> number
         ;; Returns the square root of the sum of the squares of its arguments.
         ;; (hypotenuse 3 4) -> 5

         (define (hypotenuse . numbers)
           (sqrt (fold-left (lambda (accum number)
                              (+ accum (* number number)))
                            0
                            numbers)))

         ;; (approx-eq? number number) -> boolean
         ;; Tests for approximate equality between two floating-point numbers within an absolute
         ;; or relative tolerance of EPSILON. An absolute tolerance is used for values
         ;; less than or equal to 1.0. A relative tolerance is used for larger values.
         ;; (approx-eq? 0.2 0.19999999) -> #t

         (define (approx-eq? x y)
           (let ([EPSILON 0.000001]) ;; <- arbitrary maximum allowable difference
             (<= (abs (- x y))
                 (* EPSILON 
                    (max 1.0 
                         (abs x) 
                         (abs y))))))

         ;; (assert-equal expression value) -> current-output-port
         ;; If expression does not evaluate to value, macro prints failed test to current-output-port.
         ;; (assert-equal (vec2 4 4) #(3 4)) ->
         ;; Test: (vec2 4 4)
         ;; Expect: #(3 4), Got: #(4 4)

         (define-syntax assert-equal
           (syntax-rules ()
             [(_ expression value)
              (let ([computed-expr expression]) ;; <- prevents redundant computation
                (when (not (equal? computed-expr value))
                  (printf "Test: ~a\nExpect: ~a, Got: ~a\n"
                          (quote expression) ;; <---- returns expression prior to evaluation
                          value
                          computed-expr)))]))
         )
