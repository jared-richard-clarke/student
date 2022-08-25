;; (range number number number) -> (list number ...)
;; Returns a list of numbers.
;;
;; start: optional | default: 0
;; stop:  required | if stop < start, return '()
;; step:  optional | default: 1 | if step <= 0, return '()
;;
;; (range 3)     -> '(0 1 2 3)
;; (range 1 7)   -> '(1 2 3 4 5 6 7)
;; (range 1 7 2) -> '(1 3 5 7)
;; (range 1 -2)  -> '()
;; (range 1 7 0) -> '()

(define range
  (case-lambda
    [(stop)
     (range 0 stop 1)]
    [(start stop)
     (range start stop 1)]
    [(start stop step)
     (if (<= step 0)
         '()
         (let loop ([number stop]
                    [result '()])
           (if (> start number)
               result
               (loop (- number step)
                     (cons number result)))))]))
