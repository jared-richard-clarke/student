;; (unit-circle symbol) -> list ...
;; For a unit circle, returns either one of four orthogonal points or a series of three intermediate points.
;; Points are implemented as improper lists.
;; (unit-circle '90deg)    -> '(0 . 1)
;; (unit-circle 'quad-one) -> '((0.866 . 0.5) '(0.707 . 0.707) '(0.5 . 0.866))

(define unit-circle
  (let ([0deg   '(1 . 0)]
        [30deg  '(0.866 . 0.5)]
        [45deg  '(0.707 . 0.707)]
        [60deg  '(0.5 . 0.866)]
        [90deg  '(0 . 1)]
        [120deg '(-0.5 . 0.866)]
        [135deg '(-0.707 . 0.707)]
        [150deg '(-0.866 . 0.5)]
        [180deg '(-1 . 0)]
        [210deg '(-0.866 . -0.5)]
        [225deg '(-0.707 . -0.707)]
        [240deg '(-0.5 . -0.866)]
        [270deg '(0 . -1)]
        [300deg '(0.5 . -0.866)]
        [315deg '(0.707 . -0.707)]
        [330deg '(0.866 . -0.5)])
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
