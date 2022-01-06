; Counts the nodes of a tree data structure
(define (traverse t)
  (cond
    [(null? t) 0]
    [(not (pair? t)) 1]
    [else (+ (traverse (car t))
             (traverse (cdr t)))]))
