;; (vector-fold function any vector) -> any
;; Combines vector elements piecewise into an accumulative value using function "fn".
;; The combining function takes two arguments: the accumulative value and the next
;; element in the vector. "base" provides the initial value for the accumulator.
;; (vector-fold + 0 '#(1 2 3 4 5 6 7 8 9 10)) -> 55
(define vector-fold
  (lambda (fn base xs)
    (let loop ([size (vector-length xs)]
               [index 0]
               [accum base])
      (if (>= index size)
          accum
          (loop size (+ index 1) (fn accum (vector-ref xs index)))))))

;; === Alternative ===
;;
;; (define (vector-fold fn base xs)
;;   (define (loop size index accum)
;;     (if (>= index size)
;;         accum
;;         (loop size (+ index 1) (fn accum (vector-ref xs index)))))
;;   (loop (vector-length xs) 0 base))
