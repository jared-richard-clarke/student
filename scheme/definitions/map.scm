;; map function as defined in The Scheme Programming Language.
;; Unlike the map-simple definition below, this implementation can iterate over multiple lists.
;; Both definitions omit error handling for simplificity.

(define (map fun x . xs)
  (if (null? xs)
      (let map-x ([x x])
        (if (null? x)
            '()
            (cons (fun (car x)) (map-x (cdr x)))))
      (let map-xs ([x x]
                   [xs xs])
        (if (null? x)
            '()
            (cons (apply fun (car x) (map car xs))
                  (map-xs (cdr x) (map cdr xs)))))))
