; (point number number) -> pair
; Constructs a two-dimensional point represented as a pair.
; (point 1 2) -> '(1 . 2)

(define (point x y)
  (cons x y))

; (point-distance pair pair) -> number
; Calculates the distance between two points.
; (point-distance (point 3 0) (point 2 0)) -> 1

(define point-distance
  (let ([hypotenuse (lambda (x y)
                      (sqrt (+ (sqr x) (sqr y))))])
    (lambda (p1 p2)
      (let ([x1 (car p1)]
            [y1 (cdr p1)]
            [x2 (car p2)]
            [y2 (cdr p2)])
        (hypotenuse (- x2 x1) (- y2 y1))))))

;; (path points) -> list
;; Returns a list of points.
;; (path (point 1 2) (point 3 4)) -> '((1 . 2) (3 . 4))

(define (path . points)
  points)
