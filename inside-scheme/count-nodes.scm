; Counts the nodes of a tree data structure
(define (count-nodes tree)
  (cond
    [(null? tree) 0]
    [(not (pair? tree)) 1]
    [else (+ (count-nodes (car tree))
             (count-nodes (cdr tree)))]))

; macro expansion
(define count-nodes-ex
  (lambda (tree)
    (if (null? tree)
        (begin 0)
        (if (not (pair? tree))
            (begin 1)
            (begin
              (+ (count-nodes-ex (car tree))
                 (count-nodes-ex (cdr tree))))))))
