;; (absolute number) -> number
;; Outputs the non-negative value of a number.
;; (map absolute '(1 -2 3 -4)) -> '(1 2 3 4)

(define absolute
  (lambda (x)
    (if (< x 0)
        (- 0 x)
        x)))
