;; Side Note: Using `force` and `delay` requires importing the `(rnrs r5rs)` library.

;; "The benefit of using `delay` and `force` is that some amount of computation might be
;;  avoided altogether if it is delayed until absolutely required. Delayed evaluation
;;  may be used to construct conceptually infinite lists, or streams.
;;  The example below shows how a stream abstraction may be built with `dela`y and `force`.
;;  A stream is a promise that, when forced, returns a pair whose cdr is a stream."
;;
;; — The Scheme Programming Language, 4th edition

(define stream-car
  (lambda (x)
    (car (force x))))

(define stream-cdr
  (lambda (xs)
    (cdr (force xs))))

;; If eagerly-evaluated, `counters` would never finish.

(define counters
  (let next ([n 1])
    (delay (cons n (next (+ n 1))))))

(stream-car counters) ;; --------------> 1
(stream-car (stream-cdr counters)) ;; -> 2
