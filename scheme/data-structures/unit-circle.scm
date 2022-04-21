; (point number number) -> pair
; Constructs a two-dimensional point represented as a pair.
; (point 1 2) -> '(1 . 2)

(define (point x y)
  (cons x y))

;; (unit-circle symbol) -> pair | list
;; For a unit circle, returns either one of four orthogonal points or a series of three intermediate points.
;; Points are implemented as pairs. Point series are implemented as lists.
;; (unit-circle '90deg)    -> '(0 . 1)
;; (unit-circle 'quad-one) -> '((0.866 . 0.5) '(0.707 . 0.707) '(0.5 . 0.866))

(define unit-circle
  (let ([0deg   (point 1 0)]
        [30deg  (point 0.866 0.5)]
        [45deg  (point 0.707 0.707)]
        [60deg  (point 0.5 0.866)]
        [90deg  (point 0 1)]
        [120deg (point -0.5 0.866)]
        [135deg (point -0.707 0.707)]
        [150deg (point -0.866 0.5)]
        [180deg (point -1 0)]
        [210deg (point -0.866 -0.5)]
        [225deg (point -0.707 -0.707)]
        [240deg (point -0.5 -0.866)]
        [270deg (point 0 -1)]
        [300deg (point 0.5 -0.866)]
        [315deg (point 0.707 -0.707)]
        [330deg (point 0.866 -0.5)])
    (lambda (message)
      (case message
        [(0deg) 0deg]
        [(quad-one) (list 30deg 45deg 60deg)]
        [(90deg) 90deg]
        [(quad-two) (list 120deg 135deg 150deg)]
        [(180deg) 180deg]
        [(quad-three) (list 210deg 225deg 240deg)]
        [(270deg) 270deg]
        [(quad-four) (list 300deg 315deg 330deg)]
        [else (error "invalid argument: " message)]))))
