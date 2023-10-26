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

;; range, lazily-evaluated, avoids generating a large intermediate list of integers.
;; Conceptually, range could denote an infinite list.

(define range
  (lambda (x)
    (let next ([n 1])
      (if (> n x)
          '()
          (delay (cons n (next (+ n 1))))))))

(define fold-left
  (lambda (fn accum xs)
    (if (null? xs)
        accum
        (fold-left fn
                   (fn accum (head xs))
                   (tail xs)))))
