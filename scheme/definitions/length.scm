;; (length list) -> number
;; Counts elements in proper list.
;; (length '(1 2 3)) -> 3
;; (length '()) -> 0

(define (length lst)
  (if (not (list? lst))
      (error 'length "argument not a proper list")
      (let loop ([lst lst]
                 [count 0])
        (if (null? lst)
            count
            (loop (cdr lst)
                  (+ count 1))))))
