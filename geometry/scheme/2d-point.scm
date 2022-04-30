;; Implementation of two-dimensional points and point operations.

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

;; (approximate function) -> (function point) -> point
;; Generates approximation functions for simplifying point components.
;; (define point-round (approximate round)) -> (point-round (point 1.3 1.7)) -> '(1.0 . 2.0)

(define (approximate operation)
  (lambda (pt)
    (let ([x (car pt)]
          [y (cdr pt)])
      (point (operation x) (operation y)))))

(define point-round (approximate round))
(define point-ceiling (approximate ceiling))
(define point-floor (approximate floor))

;; === testing ===

;; (assert-equal expression value) -> current-output-port
;; If expression does not evaluate to value, macro prints failed test to current-output-port.
;; (assert-equal (vec2 4 4) '(3 . 4)) ->
;; Test: (vec2 4 4)
;; Expect: (3 . 4), Got: (4 . 4)

(define-syntax assert-equal
  (syntax-rules ()
    [(_ expression value)
     (when (not (equal? expression value))
       (printf "Test: ~a\nExpect: ~a, Got: ~a\n"
               (quote expression)
               value
               expression))]))

;; === unit tests ===

(assert-equal (hypotenuse 3 4) 5)
(assert-equal (point 1 2) '(1 . 2))
(assert-equal (segment (point 3 0) (point 2 0)) 1)
(assert-equal (path (point 1 2) (point 3 4)) '((1 . 2) (3 . 4)))
(assert-equal (path-length (path (point 1 1) (point 5 1) (point 5 4) (point 1 1))) 12)
(assert-equal (point-round (point 1.3 1.7)) '(1.0 . 2.0))
(assert-equal (point-ceiling (point 1.3 1.7)) '(2.0 . 2.0))
(assert-equal (point-floor (point 1.3 1.7)) '(1.0 . 1.0))
