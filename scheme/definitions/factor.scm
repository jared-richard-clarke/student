;; (factor number) -> '(numbers ...)
;; Returns a list of the prime factors of a nonnegative integer.
;; Function is both recursive and tail recursive.
;; (factor 10) -> '(2 5)

(define (factor number)
  (let loop ([num number]
             [iter 2])
    (cond
      [(>= iter num) (list num)]
      [(integer? (/ num iter))
       (cons iter (loop (/ num iter) iter))] ;; <- recursive
      [else (loop num (+ iter 1))]))) ;; <-------- tail recursive
