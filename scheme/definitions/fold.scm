;; Fold iterates over a list, recursively building a return value through a combining operation.
;; Alternatively called reduce, compress, accumulate, aggregate, or inject.

;; (foldr function any list) -> any
;; Evaluates right to left. Stacks calls to combining operation.
;; (foldr list 'init '(a b c)) -> '(a (b (c init)))
;; (foldr cons '() '(1 2 3)) -> '(1 2 3)

(define (fold-right fn accum lst)
  (if (null? lst)
      accum
      (fn (car lst)
          (fold-right fn accum (cdr lst)))))

;; (foldl function any list) -> any
;; Evaluates left to right. Tail recursive.
;; (foldl list 'init '(a b c)) -> '(((init a) b) c)
;; (foldl cons '() '(1 2 3)) -> '(((() . 1) . 2) . 3)

(define (fold-left fn accum lst)
  (if (null? lst)
      accum
      (fold-left fn
                (fn accum (car lst))
                (cdr lst))))
