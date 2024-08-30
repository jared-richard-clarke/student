;; (map function list ...) -> list
;; Applies function to the corresponding elements of one or more lists
;; and returns a list of resulting values.
;; (map + '(1 2 3) '(1 2 3)) -> '(2 4 6)

(define (map fn x . xs)
  (if (null? xs)
      (let map-x ([x x])
        (if (null? x)
            '()
            (cons (fn (car x)) (map-x (cdr x)))))
      (let map-xs ([x x]
                   [xs xs])
        (if (null? x)
            '()
            (cons (apply fn (car x) (map car xs))
                  (map-xs (cdr x) (map cdr xs)))))))
