#lang racket

(define generate-integers
  (lambda (start stop [step 1])
    (lambda (message)
      (if (equal? message 'next)
          (if (> start stop)
              (void)
              (let ([result start])
                (set! start (+ start step))
                result))
          (error "invalid input")))))

; Yields integers from 1 through 5000 piecemeal.
(define sequence (generate-integers 1 5000))

; returns '(1 2 3)
(list (sequence 'next) (sequence 'next) (sequence 'next))

; returns '(4 5)
(list (sequence 'next) (sequence 'next))

; ...5000 then #<void>
