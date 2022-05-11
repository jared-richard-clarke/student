;; (partial function values ...) -> (function values ...) -> value
;; Fixes a number of arguments to a function, producing another function with a smaller arity.
;; (define shout (partial string-append "!")) -> (shout "doggo") -> "doggo!"

(define (partial fn . x)
  (lambda y
    (apply fn (append y x))))
