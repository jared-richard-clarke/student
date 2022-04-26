;; (hypotenuse number number) -> number
;; Computes the longest side of a right triangle.
;; (hypotenuse 3 4) -> 5

(define (hypotenuse x y)
  (sqrt (+ (sqr x) (sqr y))))

; (point number number) -> pair
; Constructs a two-dimensional point represented as a pair.
; (point 1 2) -> '(1 . 2)

(define (point x y)
  (cons x y))

;; ORIGIN: point of origin.

(define ORIGIN (point 0 0))

; (segment pair pair) -> number
; Calculates the distance between two points.
; (segment (point 3 0) (point 2 0)) -> 1

(define (segment p1 p2)
  (let ([x1 (car p1)]
        [y1 (cdr p1)]
        [x2 (car p2)]
        [y2 (cdr p2)])
    (hypotenuse (- x2 x1) (- y2 y1))))

;; (path points) -> list
;; Returns a list of points.
;; (path (point 1 2) (point 3 4)) -> '((1 . 2) (3 . 4))

(define (path . points) points)

;; (path-length path) -> number
;; Computes the length of a path.
;; (path-length (path (point 1 1) (point 5 1) (point 5 4) (point 1 1))) -> 12

(define (path-length path)
  (let loop ([sum 0]
             [pth path])
    (if (<= (length pth) 1)
        sum
        (loop (+ sum (segment (car pth) (cadr pth)))
              (cdr pth)))))
