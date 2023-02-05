;; (repeat number any) -> (list any)
;; Builds a repeated list of values for a specified length.
;; (repeat 3 '(1 2)) -> '((1 2) (1 2) (1 2))

(define repeat
  (lambda (number value)
    (let loop ([n number]
               [v value]
               [r '()])
      (if (<= n 0)
          r
          (loop (- n 1) v (cons v r))))))
