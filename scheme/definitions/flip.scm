;; (flip function) -> (function x y) -> (function y x)
;; Applies a two-argument function with its arguments flipped.
;; ((flip -) 3 10) -> 7

(define flip
  (lambda (f)
    (lambda (x y)
      (f y x))))
