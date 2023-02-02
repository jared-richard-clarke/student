;; (zip (list any) (list any)) -> (list any)
;; Combines two lists pairwise. Returns when the shortest list is exhausted.
;; (zip '(one two three) '(1 2 3)) -> '((one . 1) (two . 2) (three . 3))

(define zip
  (lambda (xs ys)
    (zip-with cons xs ys)))

;; (zip-with function (list any) (list any)) -> (list any)
;; Combines two lists pairwise, applying a function to each element pair.
;; Returns when shortest list is exhausted.
;; (zip-with + '(1 2 3 4) '(1 2 3)) -> '(2 4 6)

(define zip-with
  (lambda (fn xs ys)
    (if (or (null? xs) (null? ys))
        '()
        (cons (fn (car xs) (car ys))
              (zip-with fn (cdr xs) (cdr ys))))))
