;; Fold takes a binary function, a starting accumulator, and a list and then folds that list
;; into the accumulator using the binary function. Folds either left or right.
;; Alternatively called "reduce", "compress", "accumulate", "aggregate", or "inject".

;; (fold-right function any list) -> any
;; Folds right to left. Stacks calls to combining operation.
;; (fold-right list 'init '(a b c)) -> '(a (b (c init)))
;; (fold-right cons '() '(1 2 3)) -> '(1 2 3)

(define (fold-right fn accum xs)
  (if (null? xs)
      accum
      (fn (car xs)
          (fold-right fn accum (cdr xs)))))

;; (fold-left function any list) -> any
;; Folds left to right. Tail recursive.
;; (fold-left list 'init '(a b c)) -> '(((init a) b) c)
;; (fold-left cons '() '(1 2 3)) -> '(((() . 1) . 2) . 3)

(define (fold-left fn accum xs)
  (if (null? xs)
      accum
      (fold-left fn
                (fn accum (car xs))
                (cdr xs))))

;; === Side Note ===
;; Racket:      (foldl cons 'a '(b c)) -----> '(c b . a)
;; Chez Scheme: (fold-left cons 'a '(b c)) -> '((a . b) . c)

(define map-onto
  (lambda (fn xs ys)
    (let loop ([xs (reverse xs)]
               [ys ys])
      (if (null? xs)
          ys
          (loop (cdr xs)
                (cons (fn (car xs)) ys))))))

;; (fold function any list lists) -> any
;; This particular definition of fold takes a function, a starting
;; accumulator, and one or more lists. The function must take as
;; many arguments as their are lists plus the accumulator.
;; The accumulation terminates as soon as the shortest list has
;; been exhausted. Folds left to right.
;; (fold + 10 '(9 8 7) '(6 5 4) '(3 2 1)) -> 55

(define fold
  (lambda (fn base x . xs)
    (if (null? xs)
        (let loop ([x     x]
                   [accum base])
          (if (pair? x)
              (loop (cdr x)
                    (fn (car x) accum))
              accum))
        (let loop ([xs    (cons x xs)]
                   [accum base])
          (if (for-all pair? xs)
              (loop (map cdr xs)
                    (apply fn (map-onto car xs (list accum))))
              accum)))))
