;; (repeat any number) -> (list any)
;; Builds a repeated list of values for a specified length.
;; (repeat '(1 2) 3) -> '((1 2) (1 2) (1 2))

(define repeat
  (lambda (x n)
    (let loop ([count n]
               [result '()])
      (if (< count 1)
          result
          (loop (- count 1) (cons x result))))))
