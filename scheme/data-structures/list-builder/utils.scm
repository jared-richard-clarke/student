(library (utils)
         (export assert range)
         (import (rnrs base)
                 (rnrs control)
                 (rnrs io simple))
         
         (define-syntax assert
           (syntax-rules ()
             [(_ compare x y)
              (let ([computed-x x]
                    [computed-y y])
                (unless (compare computed-x computed-y)
                  (begin (display "Test failed:")
                         (newline)
                         (display "lhs: ") (write (quote x)) (display " -> ") (write computed-x) (display ", ")
                         (display "rhs: ") (write (quote y)) (display " -> ") (write computed-y)
                         (newline))))]))

         (define range
           (case-lambda
             [(stop)
              (range 0 stop 1)]
             [(start stop)
              (range start stop 1)]
             [(start stop step)
              (if (<= step 0)
                  '()
                  (let recur ([start start]
                              [stop  stop]
                              [step  step])
                    (if (> start stop)
                        '()
                        (cons start (recur (+ start step) stop step)))))]))
         )
