;; === Primitives ===

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

;; (lazy-list value...) -> <promise>
;; Transforms a variable number of values into a lazy list.
(define lazy-list
  (lambda xs
    (if (null? xs)
        '()
        (delay (cons (car xs) (apply lazy-list (cdr xs)))))))

;; === Generators ===

;; (range start) | (range start stop) | (range start stop step) -> <promise>
;;     where start, stop, step = number
;;     and start < stop
;;     and step > 0
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
     (if (or (>= start stop) (<= step 0))
         '()
         (let next ([x start])
           (if (> x stop)
               '()
               (delay (cons x (next (+ x step)))))))]))

;; (append <promise> <promise>) -> <promise>
;; Returns a promise to append two lazy lists.
;; (take 4 (append (lazy-list 1 2 3) (lazy-list 4 5 6))) -> '(1 2 3 4)
(define append
  (lambda (xs ys)
    (if (null? xs)
        ys
        (delay (cons (head xs) (append (tail xs) ys))))))

;; (repeat any) -> <promise>
;; Returns a promise to build an infinite list of a repeated value.
;; (take 4 (repeat 'x)) -> '(x x x x)
(define repeat
  (lambda (x)
    (delay (cons x (repeat x)))))

;; (iterate function any) -> <promise>
;; Returns a promise to build an infinite list of a value repeatedly
;; iterated over by the provided function.
;; (take 5 (iterate add1 5)) -> '(5 6 7 8 9)
(define iterate
  (lambda (f x)
    (delay (cons x (iterate f (f x))))))

;; (cycle <promise>) -> <promise>
;; Returns a promise to build a circular list from a finite one.
;; (take 7 (cycle (lazy-list 1 2 3))) -> '(1 2 3 1 2 3 1)
(define cycle
  (lambda (xs)
    (let next ([ys xs])
      (delay (if (null? ys)
                 (cons (head xs) (next (tail xs)))
                 (cons (head ys) (next (tail ys))))))))

;; (map function <promise>) -> <promise>
;; Returns a promise to map an arbitrary function over a lazy list.
;; (take 4 (map add1 (cycle '(1 2)))) -> '(2 3 2 3)
(define map
  (lambda (f xs)
    (if (null? xs)
        '()
        (delay (cons (f (head xs)) (map f (tail xs)))))))

;; === Consumers ===

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
;; Combines two promises to build lists pairwise, applying an arbitrary function
;; to each element pair. Returns when shortest list is exhausted.
;; (zip-with + (repeat 3 3) (repeat 4 100)) -> '(7 7 7)
(define zip-with
  (lambda (fn xs ys)
    (if (or (null? xs) (null? ys))
        '()
        (cons (fn (head xs) (head ys))
              (zip-with fn (tail xs) (tail ys))))))
