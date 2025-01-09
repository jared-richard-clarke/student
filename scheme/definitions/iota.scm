;; (iota count)            -> (list 0 ... (+ 0 (* (- count 1) 1)))
;; (iota count start)      -> (list start ... (+ start (* (- count 1) 1)))
;; (iota count start step) -> (list start ... (+ start (* (- count 1) step)))
;;   where count = number
;;         start = number
;;         step  = number
;;
;; Returns a list of length "count" containing a sequence of numbers
;; ordered according to "start" and "step".
;; The "start" and "step" parameters default to 0 and 1 respectively.
;;
;; (iota 10)     -> '(0 1 2 3 4  5  6  7  8  9)
;; (iota 10 1)   -> '(1 2 3 4 5  6  7  8  9 10)
;; (iota 10 1 2) -> '(1 3 5 7 9 11 13 15 17 19)

(define iota
  (case-lambda
   [(count)
    (iota count 0 1)]
   [(count start)
    (iota count start 1)]
   [(count start step)
    (let loop ([counter count]
               [result  '()])
      (if (<= counter 0)
          result
          (loop (- counter 1)
                (cons (+ start (* (- counter 1) step)) result))))]))
