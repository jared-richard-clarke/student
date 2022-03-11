;; (accumulate function value list) -> value
;; Iterates over a sequence, accumulating the results into a single value.
;; (accumulate + 0 '(1 2 4)) -> 7

(define (accumulate action init sequence)
  (if (null? sequence)
      init
      (action (car sequence)
          (accumulate action init (cdr sequence)))))
