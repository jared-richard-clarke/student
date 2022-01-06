; Counts the nodes of a tree data structure
(define (traverse tree)
  (cond
    [(null? tree) 0]
    [(not (pair? tree)) 1]
    [else (+ (traverse (car tree))
             (traverse (cdr tree)))]))
