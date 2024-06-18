;; (traverse list function) -> list
;; Traverses sequences, both flat and recursive, applying
;; an arbitrary function to each one of its nodes.

(define (traverse tree fn)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (traverse sub-tree fn)
             (fn sub-tree)))
       tree))
