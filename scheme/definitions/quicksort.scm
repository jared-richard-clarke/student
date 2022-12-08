;; (quick-sort (list number ...)) -> (list number ...)
;; Sorts a list of numbers from least to greatest using the quick-sort algorithm.
;; (quick-sort '(3 -4 11 2 11)) -> '(-4 2 3 11 11)

(define (le x)
  (lambda (y) (<= y x)))

(define (gt x)
  (lambda (y) (> y x)))

(define (quick-sort lst)
  (if (null? lst)
      '()
      (let ([x  (car lst)]
            [xs (cdr lst)])
        (let ([left  (quick-sort (filter (le x) xs))]
              [right (quick-sort (filter (gt x) xs))])
          (append left (list x) right)))))
