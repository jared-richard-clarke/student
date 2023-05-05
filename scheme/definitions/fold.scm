;; Fold takes a binary function, a starting accumulator, and a list and then folds that list
;; into the accumulator from the left or right using the binary function.
;; Alternatively called "reduce", "compress", "accumulate", "aggregate", or "inject".

;; (fold-right function any list) -> any
;; Folds right to left. Stacks calls to combining operation.
;; (fold-right list 'init '(a b c)) -> '(a (b (c init)))
;; (fold-right cons '() '(1 2 3)) -> '(1 2 3)

(define (fold-right fn accum lst)
  (if (null? lst)
      accum
      (fn (car lst)
          (fold-right fn accum (cdr lst)))))

;; (fold-left function any list) -> any
;; Folds left to right. Tail recursive.
;; (fold-left list 'init '(a b c)) -> '(((init a) b) c)
;; (fold-left cons '() '(1 2 3)) -> '(((() . 1) . 2) . 3)

(define (fold-left fn accum lst)
  (if (null? lst)
      accum
      (fold-left fn
                (fn accum (car lst))
                (cdr lst))))

;; === Side Note ===
;; Racket:      (foldl cons 'a '(b c)) -----> '(c b . a)
;; Chez Scheme: (fold-left cons 'a '(b c)) -> '((a . b) . c)
