;; map function as defined in The Scheme Programming Language.
;; Unlike the map-simple definition below, this implementation can iterate over multiple lists.
;; Both definitions omit error handling for simplificity.

(define map
  (lambda (func lst . more)
    (if (null? more)
        (let map1 ([lst lst])
          (if (null? lst)
              '()
              (cons (func (car lst))
                    (map1 (cdr lst)))))
        (let map-more ([lst lst]
                       [more more])
          (if (null? lst)
              '()
              (cons (apply func
                           (car lst)
                           (map car more))
                    (map-more (cdr lst)
                              (map cdr more))))))))

;; (map-simple function list) -> list
;; Constructs a new sequence populated with the results of calling 
;; the provided function on each element of the provided sequence.
;; (map-simple add-1 '(1 2 3)) -> '(2 3 4)

(define (map-simple action sequence)
  (if (null? sequence)
      null
      (cons (action (car sequence))
            (map-simple action (cdr sequence)))))
