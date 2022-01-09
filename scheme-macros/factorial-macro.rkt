#lang racket

(define-syntax function
  (syntax-rules ()
    [(_ x y z ...)
     (lambda x y z ...)]))

(define factorial
  (function (x)
    (if (< x 1)
        1
        (* x (factorial (- x 1))))))
