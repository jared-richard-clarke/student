;; traverse tree
;; Many tree operations can be implemented via combinations
;; of sequence operations and recursion.

(define (traverse-1 tree action)
  (cond [(null? tree) null]
        [(not (pair? tree)) (action tree)]
        [else (cons (traverse-1 (car tree) action)
                    (traverse-1 (cdr tree) action))]))

;; traverse tree: map implementation
;; Regards tree as a sequence of sub-trees.

(define (traverse tree action)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (traverse sub-tree action)
             (action sub-tree)))
       tree))
