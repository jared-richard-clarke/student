;; (hypotenuse number number) -> number
;; Returns the square root of the sum of the squares of its arguments.
;; (hypotenuse 3 4) -> 5

(define (hypotenuse . numbers)
  (sqrt (fold-left (lambda (accum number)
                     (+ accum (sqr number)))
                   0
                   numbers)))
