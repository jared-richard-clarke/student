;; (accumulate function value list) -> value
;; Iterates over a list, recursively building a return value through a combining operation.
;; Alternatively called fold, reduce, compress, aggregate, or inject.
;; (accumulate cons '() '(1 2 4)) -> '(1 2 4)
;; (accumulate + 0 '(1 2 4)) -> 7

(define (accumulate action init sequence)
  (if (null? sequence)
      init
      (action (car sequence)
          (accumulate action init (cdr sequence)))))
