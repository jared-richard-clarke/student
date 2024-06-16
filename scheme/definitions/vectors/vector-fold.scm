;; (vector-fold-left function any vector vectors ...) -> any
;; Combines vector elements piecewise, left to right, into an accumulative value using function "fn".
;; The combining function takes the accumulative value and the next element
;; from one or more vectors. "base" provides the initial value for the accumulator.
;; The number of elements combined match the length of the initial vector.
;; (vector-fold-left + 0 #(1 2 3) #(4 5 6) #(7 8 9)) -> 45

(define vector-fold-left
  (lambda (fn base v . vs)
    (let ([size (vector-length v)])
      (if (null? vs)
          (let loop ([index 0]
                     [accum base])
            (if (>= index size)
                accum
                (loop (+ index 1) (fn accum (vector-ref v index)))))
          (let loop ([index 0]
                     [accum base])
            (if (>= index size)
                accum
                (loop (+ index 1)
                      (apply fn accum (vector-ref v index)
                             (map (lambda (x) (vector-ref x index)) vs)))))))))

;; (vector-fold-right function any vector vectors ...) -> any
;; Combines vector elements piecewise, right to left, into an accumulative value using function "fn".
;; The combining function takes the accumulative value and the next element from one or more vectors.
;; "base" provides the initial value for the accumulator. The number of elements combined match the
;; length of the initial vector.
;; (vector-fold-right list 'base #(1 2 3) #(4 5 6) #(7 8 9)) -> '(((base 3 6 9) 2 5 8) 1 4 7)
;; (vector-fold-left  list 'base #(1 2 3) #(4 5 6) #(7 8 9)) -> '(((base 1 4 7) 2 5 8) 3 6 9)

(define vector-fold-right
  (lambda (fn base v . vs)
    (let ([size (vector-length v)])
      (if (null? vs)
          (let loop ([index (- size 1)]
                     [accum base])
            (if (< index 0)
                accum
                (loop (- index 1) (fn accum (vector-ref v index)))))
          (let loop ([index (- size 1)]
                     [accum base])
            (if (< index 0)
                accum
                (loop (- index 1)
                      (apply fn accum (vector-ref v index)
                             (map (lambda (x) (vector-ref x index)) vs)))))))))
