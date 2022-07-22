;; (identity any) -> any
;; The identity combinator. Implemented as a function that returns its argument unchanged.
;; (identity 7) -> 7
;; (identity '(1 2 3)) -> '(1 2 3)

(define (identity x) x)
