#lang racket
; Racket: Scheme dialect
(define sigma
  (lambda (operation start stop)
    (if (> start stop)
        0
        (+ (operation start)
           (sigma operation (+ start 1) stop)))))

; result = 30
(define result
  (sigma (lambda (x)
           (* x x))
         1
         4))
