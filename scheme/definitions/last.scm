;; (last list) -> any
;; Returns the last element in a non-empty list.
;; Returns an empty list otherwise.
;; (last '(1 2 3)) -> 3
(define last
  (lambda (xs)
    (if (null? xs)
        '()
        (let loop ([ln (length xs)]
                   [ys xs])
          (if (= ln 1)
              (car ys)
              (loop (- ln 1) (cdr ys)))))))
