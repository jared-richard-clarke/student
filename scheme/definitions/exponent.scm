;; (pow number number) -> number
;; Raises x to the power of y.
;; (pow 2 4) -> 16

(define (pow x y)
    (let ([MUL-ID 1]
          [X (if (negative? y) (/ 1 x) x)])
      (if (= y 0)
          MUL-ID  
          (let loop ([product X]
                     [count (abs y)])
            (if (<= count 1)
                product
                (loop (* product X) (- count 1)))))))

;; (^ number ...) -> number
;; Raises subsequent xs to the power of x, where x 
;; is the product of x raised to the power of x. Is right associative.
;; (^ 2 2 2) -> (pow 2 (pow 2 2)) -> 16

(define (^ . xs)
    (fold-right pow 1 xs))
