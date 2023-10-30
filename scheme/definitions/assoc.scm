(define (find compare)
  (lambda (x xs)
    (let loop ([xs xs])
      (cond
        [(null? xs) #f]
        [(compare (caar xs) x) (car xs)]
        [else (loop (cdr xs))]))))

;; (assoc any list) -> pair or boolean
;; Either finds first element of an associative list whose car equals obj or returns false.
;; assv and assq are defined similarly, with eqv? and eq? in place of equal?
;; (assoc 'b '((a . 1) (b . 2))) -> (b . 2)

(define assoc (find equal?))

(define assq (find eq?))

(define assv (find eqv?))
