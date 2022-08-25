;; (range number number [number]) -> (list number ...)
;; Generates a list of numbers, ranging from its start to stop parameter.
;; Default step parameter is 1.
;; (range 1 7) -> '(1 2 3 4 5 6 7)
;; (range 1 7 2) -> '(1 3 5 7)

(define range
  (case-lambda
    [(start stop)
     (range start stop 1)]
    [(start stop step)
     (let loop ([number stop]
                [result '()])
       (if (> start number)
           result
           (loop (- number step)
                 (cons number result))))]))
