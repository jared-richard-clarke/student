;; Fold takes a binary function, a starting state, and a list and then folds
;; that list into the state using the binary function. Folds either left or right.
;; Alternatively called "reduce", "compress", "accumulate", "aggregate", or "inject".

;; (fold-right function any list) -> any
;; Folds right to left. Stacks calls to combining operation.
;; (fold-right list 'init '(a b c)) -> '(a (b (c init)))
;; (fold-right cons '() '(1 2 3)) -> '(1 2 3)
(define fold-right
  (lambda (fn state xs)
    (if (null? xs)
        state
        (fn (car xs) (fold-right fn state (cdr xs))))))

;; (reduce-right function list) -> any
;; Like "fold-right" but the last element in the list
;; is the base state. Consequently, the list must
;; be non-empty.
;; (reduce-right cons '(1 2 3)) -> '(1 2 . 3)
(define reduce-right
  (lambda (fn xs)
    (fold-right (lambda (x state)
                  (if (null? state)
                      x
                      (fn x state)))
                '()
                xs)))

;; (fold-left function any list) -> any
;; Folds left to right. Tail recursive.
;; (fold-left list 'init '(a b c)) -> '(((init a) b) c)
;; (fold-left cons '() '(1 2 3)) -> '(((() . 1) . 2) . 3)
(define fold-left
  (lambda (fn state xs)
    (if (null? xs)
        state
        (fold-left fn (fn state (car xs)) (cdr xs)))))

;; (reduce function list) -> any
;; Like "fold-left" but the first element in the list
;; is the base state. Consequently, the list must
;; be non-empty.
;; (reduce cons '(1 2 3)) -> '((1 . 2) . 3)
(define reduce
  (lambda (fn xs)
    (let ([head (car xs)]
          [tail (cdr xs)])
      (fold-left fn head tail))))

;; === Side Note ===
;; Racket:      (foldl cons 'a '(b c)) -----> '(c b . a)
;; Chez Scheme: (fold-left cons 'a '(b c)) -> '((a . b) . c)

;; (fold function any list lists) -> any
;; This particular definition of fold takes a function, a starting
;; state, and one or more lists. The function must take as
;; many arguments as their are lists plus the current state.
;; The accumulation terminates as soon as the shortest list has
;; been exhausted. Folds left to right.
;; (fold + 10 '(9 8 7) '(6 5 4) '(3 2 1))       -> 55
;; (fold list 'init '(1 2 3) '(3 4 5) '(6 7 8)) -> (((init 1 3 6) 2 4 7) 3 5 8)
(define fold
  (lambda (fn base x . xs)
    (if (null? xs)
        (let fold-x ([state base] [x x])
          (if (pair? x)
              (fold-x (fn state (car x)) (cdr x))
              state))
        (let fold-xs ([state base] [xs (cons x xs)])
          (if (for-all pair? xs)
              (fold-xs (apply fn state (map car xs)) (map cdr xs))
              state)))))
