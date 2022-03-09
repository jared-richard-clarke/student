
;; === Newton's Method for approximating square roots ===
;; √x = the y such that y ≥ 0 and y ^ 2 = x
;; Average y with x ÷ y in successive approximations — each average
;; closer than the last.
;; guess        quotient                 average
;; 1            (2 ÷ 1) = 2              ((2 + 1) ÷ 2) = 1.5
;; 1.5          (2 ÷ 1.5) = 1.3333       ((1.3333 ÷ 1.5) ÷ 2) = 1.4167
;; 1.4167 ...

(define (square-root x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

;; === fixed point ===
;; (fixed-point function number) -> number
;; A number x is called a fixed point of a function ƒ if x satisfies  
;; the equation ƒ(x) = x. For some functions ƒ we can locate a fixed point
;; by beginning with an initial guess and applying ƒ repeatedly.

(define (fixed-point func first-guess)
  
  (define TOLERANCE 0.00001)
  
  (define (close? x y)
    (< (abs (- x y)) TOLERANCE))

  (define (try guess)
    (let ([next (func guess)])
      (if (close? guess next)
          next
          (try next))))
  
  (try first-guess))

;; === average-damp ===
;; (average-damp function) -> (function number) -> number
;; Average damping forces successive approximations to converge
;; where they might otherwise loop infinitely.

(define (average-damp func)
  (let ([average (lambda (a b)
                   (/ (+ a b) 2))])
    (lambda (x) (average x (func x)))))
