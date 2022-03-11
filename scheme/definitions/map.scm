;; map function as defined in The Scheme Programming Language.
;; Unlike the map-simple definition below, this implementation can iterate over multiple lists.
;; Both definitions omit error handling for simplificity.

(define map
  (lambda (action seq . more)
    (if (null? more)
        (let map-1 ([seq seq])
          (if (null? seq)
              '()
              (cons (action (car seq))
                    (map-1 (cdr seq)))))
        (let map-more ([seq seq]
                       [more more])
          (if (null? seq)
              '()
              (cons (apply action
                           (car seq)
                           (map car more))
                    (map-more (cdr seq)
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
