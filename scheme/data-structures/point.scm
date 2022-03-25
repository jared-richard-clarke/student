; (point number number) -> pair
; Constructs a two-dimensional point represented as a pair.
; (point 1 2) -> '(1 . 2)

(define (point x y)
  (cons x y))

; (point-distance pair pair) -> number
; Calculates the distance between two points.
; (point-distance '(3, 0) '(2, 0)) -> 1

(define point-distance
  (let ([hypotenuse (lambda (x y)
                      (sqrt (+ (sqr x) (sqr y))))])
    (lambda (x y)
      (let ([x1 (car x)]
            [x2 (cdr x)]
            [y1 (car y)]
            [y2 (cdr y)])
        (hypotenuse (- y1 x1) (- y2 x2))))))

;; (path points) -> list of point pairs
;; Returns a list of points.
;; (path (point 1 2) (point 3 4)) -> '((1 . 2) (3 . 4))

(define (path . points)
  points)

;; (path-length path) -> number
;; Computes the path length of a series of paths.
;; (path-length (path (point 1 1) (point 5 1) (point 5 4) (point 1 1))) -> 12

(define (path-length path)
  (let loop ([sum 0]
             [len (length path)]
             [path path])
    (if (= len 1)
        sum
        (let ([x (car path)]   ;; first
              [y (cadr path)]) ;; second
          (loop (+ sum (point-distance x y))
                (- len 1)
                (cdr path))))))
