;; (head <promise>) -> value
;; Returns the head of a lazy cons list.
(define head
  (lambda (x)
    (car (force x))))

;; (tail <promise>) -> <promise>
;; Returns the tail of a lazy cons list.
(define tail
  (lambda (xs)
    (cdr (force xs))))

;; (range start) | (range start stop) | (range start stop step) -> <promise>
;;     where start, stop, step = number
;; Returns a promise to build a list enumerating a selected range of numbers.
;; "range", lazily-evaluated, avoids generating an intermediate list.
;; Conceptually, "range" could denote an infinite list.
(define range
  (case-lambda
    [(stop)
     (range 1 stop 1)]
    [(start stop)
     (range start stop 1)]
    [(start stop step)
     (if (or (> start stop) (<= step 0))
         '()
         (let next ([x start])
           (if (> x stop)
               '()
               (delay (cons x (next (+ x step)))))))]))

;; (repeat any number) -> <promise>
;; Returns a promise to build a repeated list of values for a specified length.
;; (take 3 (repeat "repeat" 100)) -> '("repeat" "repeat" "repeat")
(define repeat
  (lambda (x n)
    (let next ([counter n])
      (if (< counter 1)
          '()
          (delay (cons x (next (- counter 1))))))))

;; (take number (range number)) -> (list number)
;; Takes the first "n" elements of range. Takes the
;; whole list if "n" is greater than the length of range.
;; (take 2 (range 10)) -> '(1 2)
(define take
  (lambda (n xs)
    (if (or (<= n 0) (null? xs))
        '()
        (cons (head xs) (take (- n 1) (tail xs))))))

;; (drop number (range number)) -> <promise>
;; Drops the first "n" elements of range. Drops the
;; whole list if "n" is greater than the length of range.
;; (take 1 (drop 2 (range 10))) -> '(3)
(define drop
  (lambda (n xs)
    (if (or (<= n 0) (null? xs))
        xs
        (drop (- n 1) (tail xs)))))

;; (take-while function (range number)) -> (list number)
;; Returns the longest possible prefix list of list range
;; that satisfies predicate "test".
;; (take-while (lambda (x) (< x 5)) (range 10)) -> '(1 2 3 4)
(define take-while
  (lambda (test xs)
    (let ([head (head xs)]
          [tail (tail xs)])
      (if (or (null? tail) (not (test head)))
          '()
          (cons head (take-while test tail))))))

;; (drop-while function (range number)) -> <promise>
;; Returns the remaining promise of range that doesn't
;; satisfy the predicate.
;; (take 1 (drop-while (lambda (x) (< x 5)) (range 10))) -> '(5)
(define drop-while
  (lambda (test xs)
    (let ([head (head xs)]
          [tail (tail xs)])
      (if (or (null? tail) (not (test head)))
          xs
          (drop-while test tail)))))

;; (zip <promise> <promise>) -> (list any)
;; Combines two promises to build lists pairwise.
;; Returns when the shortest promise is exhausted.
;; (zip (repeat 7 2) (range 10)) -> '((7 . 1) (7 . 2))
(define zip
  (lambda (xs ys)
    (zip-with cons xs ys)))

;; (zip-with function <promise> <promise>) -> (list any)
;; Combines two promises to build lists pairwise, applying an arbritrary function
;; to each element pair. Returns when shortest list is exhausted.
;; (zip-with + (repeat 3 3) (repeat 4 100)) -> '(7 7 7)
(define zip-with
  (lambda (fn xs ys)
    (if (or (null? xs) (null? ys))
        '()
        (cons (fn (head xs) (head ys))
              (zip-with fn (tail xs) (tail ys))))))
