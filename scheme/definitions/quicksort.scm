;; (quick-sort function (list any ...)) -> (list any ...)
;; Sorts a list of comparable items using the quick-sort algorithm.
;; The "compare" function sorts items to the left of the pivot, whereas
;; the the inverse of "compare" sorts to the right. Thereby "compare"
;; determines ascending or descending order for elements in the list.
;; (quick-sort < '(3 -4 11 2 11)) ---------> '(-4 2 3 11 11)
;; (quick-sort char<? '(#\b #\c #\a #\d)) -> '(#\a #\b #\c #\d)

(define (quick-sort compare lst)
  (if (null? lst)
      '()
      (let ([x  (car lst)]
            [xs (cdr lst)])
        (let ([left  (quick-sort compare (filter (lambda (y) (compare y x)) xs))]
              [right (quick-sort compare (filter (lambda (y) (not (compare y x))) xs))])
          (append left (list x) right)))))
