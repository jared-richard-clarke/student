;; (zip function list list) -> list
;; Combines two lists, element by element. Returns when shortest list is exhausted.
;; (zip cons '(one two three) '(1 2 3)) -> '((one . 1) (two . 2) (three . 3))
;; (zip + '(1 2 3 4) '(1 2 3)) -> '(2 4 6)

(define (zip fn l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (fn (car l1) (car l2))
            (zip fn (cdr l1) (cdr l2)))))
