#lang racket
; === constructor ===
(define (new-stack)
  ; === object instance ===
  (define stack '())
  ; === methods ===
  ; empty?: empty stack? true or false
  (define (empty?)
    (null? stack))
  ; push!: adds one or more elements to the end of stack and returns the new length of stack.
  (define (push! args)
    (set! stack (foldr cons stack args))
    (length stack))
  ; peek: shows last element added to stack.
  (define (peek)
    (car stack))
  ; pop!: removes last element from stack and returns that element.
  (define (pop!)
    (let ([item (car stack)])
      (set! stack (cdr stack))
      item))
  ; === interface === 
  (lambda (message . arguments)
    (cond
      [(eqv? message 'empty?) (empty?)]
      [(eqv? message 'push!) (push! arguments)]
      [(eqv? message 'peek) (peek)]
      [(eqv? message 'pop!) (pop!)]
      [else (error "invalid input")])))
