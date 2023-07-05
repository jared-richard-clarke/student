(define (new-stack)
  ;; === instance ===
  (define stack '())
  ;; === methods ===
  ;; identifier
  (define type 'stack)
  ;; empty?: empty stack? true or false
  (define (empty?)
    (null? stack))
  ;; push!: adds one or more elements to the end of stack.
  (define (push! args)
    (set! stack (append args stack)))
  ;; peek: shows last element added to stack.
  (define (peek)
    (car stack))
  ;; pop!: removes last element from stack and returns that element.
  (define (pop!)
    (let ([item (car stack)])
      (set! stack (cdr stack))
      item))
  ;; clear!: sets stack to empty.
  (define (clear!)
    (set! stack '()))
  ; === interface === 
  (lambda (message . arguments)
    (cond
      [(eq? message 'type)    type]
      [(eq? message 'empty?) (empty?)]
      [(eq? message 'push!)  (push! arguments)]
      [(eq? message 'peek)   (peek)]
      [(eq? message 'pop!)   (pop!)]
      [(eq? message 'clear!) (clear!)]
      [else (error "invalid input:" message)])))
