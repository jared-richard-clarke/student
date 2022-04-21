;; (unit-circle symbol) -> pair
;; For a series of angles, returns the corresponding coordinates of a unit circle.
;; (unit-circle '90deg)  -> '(0 . 1)
;; (unit-circle '180deg) -> '(-1 . 0)

(define unit-circle
  (let ([table '([0deg   (1 . 0)]
                 [30deg  (0.866 . 0.5)]
                 [45deg  (0.707 . 0.707)]
                 [60deg  (0.5 . 0.866)]
                 [90deg  (0 . 1)]
                 [120deg (-0.5 . 0.866)]
                 [135deg (-0.707 . 0.707)]
                 [150deg (-0.866 . 0.5)]
                 [180deg (-1 . 0)]
                 [210deg (-0.866 . -0.5)]
                 [225deg (-0.707 . -0.707)]
                 [240deg (-0.5 . -0.866)]
                 [270deg (0 . -1)]
                 [300deg (0.5 . -0.866)]
                 [315deg (0.707 . -0.707)]
                 [330deg (0.866 . -0.5)])])
    (lambda (key)
      (let ([value (assq key table)])
        (if (eq? value #f)
            (error "invalid key: " key)
            (cadr value))))))
