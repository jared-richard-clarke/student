; (range number number number) -> list
; Generates a list of numbers. Default step is 1.
; (range 1 10) -> '(1 2 3 4 5 6 7 8 9 10)

(define (range start stop [step 1])
  (let loop ([count stop]
             [result '()])
    (if (> start count)
        result
        (loop (- count step)
              (cons count result)))))
