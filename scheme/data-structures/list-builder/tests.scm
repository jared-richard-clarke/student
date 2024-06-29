(import (rnrs base)
        (list-builder)
        (utils))

(define binary '(0 1))

(define count-to-fifteen
  (for ([bit-4 <- binary]
        [bit-3 <- binary]
        [bit-2 <- binary]
        [bit-1 <- binary])
    (list bit-4 bit-3 bit-2 bit-1)))

(define (py-triple n)
  (for ([x <- (range 1 n)]
        [y <- (range x n)]
        [z <- (range y n)])
    (when (= (+ (sqr x) (sqr y))
             (sqr z)))
    (list x y z)))

;; === tests ===

(assert equal?
        count-to-fifteen
        '((0 0 0 0) (0 0 0 1) (0 0 1 0) (0 0 1 1)
          (0 1 0 0) (0 1 0 1) (0 1 1 0) (0 1 1 1)
          (1 0 0 0) (1 0 0 1) (1 0 1 0) (1 0 1 1)
          (1 1 0 0) (1 1 0 1) (1 1 1 0) (1 1 1 1)))

(assert equal?
        (py-triple 21)
        '((3 4 5) (5 12 13) (6 8 10)
          (8 15 17) (9 12 15) (12 16 20)))
