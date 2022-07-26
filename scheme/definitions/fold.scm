;; Fold, right and left, iterate over a list, recursively building a return value through a combining operation.
;; Alternatively called reduce, compress, accumulate, aggregate, or inject.

;; (foldr function any list) -> any
;; Evaluates right to left. Stacks calls to combining operation.
;; (foldr list 'init '(a b c)) -> '(a (b (c init)))
;; (foldr cons '() '(1 2 3)) -> '(1 2 3)

(define (foldr fn accum lst)
  (if (null? lst)
      accum
      (fn (car lst)
          (foldr fn accum (cdr lst)))))

;; (foldl function any list) -> any
;; Evaluates left to right. Tail recursive.
;; (foldl list 'init '(a b c)) -> '(c (b (a init)))
;; (foldl cons '() '(1 2 3)) -> '(3 2 1)

(define (foldl fn accum lst)
  (if (null? lst)
      accum
      (foldl fn
             (fn (car lst) accum)
             (cdr lst))))
