#lang racket

; (factorial number) -> number
; Computes the product of a positive integer and all the positive integers below it.
; (factorial 4) -> 24

(define (factorial x)
  (apply * (range 1 x)))

; === helper: range ===
(define (range start stop [step 1])
  (let loop ([count stop]
             [result '()])
    (if (> start count)
        result
        (loop (- count step)
              (cons count result)))))
