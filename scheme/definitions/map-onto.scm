;; (map-onto function list any)
;; Map a function onto a list while simultaneously
;; appending its output onto the front of another value,
;; usually another list.
;; (map-onto add1 '(0 1 2) '(4 5 6)) -> '(1 2 3 4 5 6)

(define map-onto
  (lambda (fn xs ys)
    (let loop ([xs (reverse xs)]
               [ys ys])
      (if (null? xs)
          ys
          (loop (cdr xs)
                (cons (fn (car xs)) ys))))))
