;; Side Note: Using `force` and `delay` requires importing the `(rnrs r5rs)` library.

;; "The benefit of using `delay` and `force` is that some amount of computation might be
;;  avoided altogether if it is delayed until absolutely required. Delayed evaluation
;;  may be used to construct conceptually infinite lists, or streams.
;;  The example below shows how a stream abstraction may be built with `delay` and `force`.
;;  A stream is a promise that, when forced, returns a pair whose `cdr` is a stream."
;;
;; — The Scheme Programming Language, 4th edition

(define head
  (lambda (x)
    (car (force x))))

(define tail
  (lambda (xs)
    (cdr (force xs))))

;; "range", lazily-evaluated, avoids generating an intermediate list of integers.
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

(define fold-left
  (lambda (fn accum xs)
    (if (null? xs)
        accum
        (fold-left fn
                   (fn accum (head xs))
                   (tail xs)))))

(define factorial
  (lambda (n)
    (fold-left * 1 (range 2 n))))
