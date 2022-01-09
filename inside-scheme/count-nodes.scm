; Counts the nodes of a tree data structure
(define (count-nodes tree)
  (cond
    [(null? tree) 0]
    [(not (pair? tree)) 1]
    [else (+ (count-nodes (car tree))
             (count-nodes (cdr tree)))]))
