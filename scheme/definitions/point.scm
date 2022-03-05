; (point number number) -> pair
; Constructs a two-dimensional point represented as a pair.
; (point 1 2) -> '(1 . 2)

(define (point x y)
  (cons x y))

; (distance pair pair) -> number
; Calculates the distance between two points.
; (distance '(3, 0) '(2, 0)) -> 1

(define distance
  (let ([hypotenuse (lambda (x y)
                      (sqrt (+ (sqr x) (sqr y))))])
    (lambda (x y)
      (let ([x1 (car x)]
            [x2 (cdr x)]
            [y1 (car y)]
            [y2 (cdr y)])
        (hypotenuse (- x1 y1) (- x2 y2))))))
