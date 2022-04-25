;; (hypotenuse number number) -> number
;; Computes the longest side of a right triangle.
;; (hypotenuse 3 4) -> 5

(define (hypotenuse x y)
  (sqrt (+ (sqr x) (sqr y))))

; (point2D number number) -> pair
; Constructs a two-dimensional point represented as a pair.
; (point2D 1 2) -> '(1 . 2)

(define (point2D x y)
  (cons x y))

; (point2D-length pair pair) -> number
; Calculates the distance between two points.
; (point2D-length (point2D 3 0) (point2D 2 0)) -> 1

(define (point2D-length p1 p2)
  (let ([x1 (car p1)]
        [y1 (cdr p1)]
        [x2 (car p2)]
        [y2 (cdr p2)])
    (hypotenuse (- x2 x1) (- y2 y1))))

;; (path2D points) -> list
;; Returns a list of points.
;; (path2D (point2D 1 2) (point2D 3 4)) -> '((1 . 2) (3 . 4))

(define (path2D . points) points)

;; (path2D-length path) -> number
;; Computes the length of a path.
;; (path2D-length (path2D (point2D 1 1) (point2D 5 1) (point2D 5 4) (point2D 1 1))) -> 12

(define (path2D-length path)
  (let loop ([sum 0]
             [pth path])
    (if (<= (length pth) 1)
        sum
        (loop (+ sum (point2D-length (car pth) (cadr pth)))
              (cdr pth)))))
