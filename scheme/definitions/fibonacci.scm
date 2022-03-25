;; Fibonacci sequence: a sequence in which each number is the sum of the preceding two.
;; (0, 1, 1, 2, 3, 5, 8, 13, 21, 34, ...)

;; (fibonacci number) -> number
;; Returns the nth term of a fibonacci sequence. Starts at 0.
;; (fibonacci 4) -> 3

(define (fibonacci nth)
  (let loop ([1st 0]
             [2nd 1]
             [iter 0])
    (if (>= iter nth)
        1st
        (loop 2nd (+ 1st 2nd) (+ iter 1)))))

;; do-loop implementation

(define (fib nth)
  (do ([1st 0 2nd]
       [2nd 1 (+ 1st 2nd)]
       [iter 0 (+ iter 1)])
    ((>= iter nth) 1st)))
