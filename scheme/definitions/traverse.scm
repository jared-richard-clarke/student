;; (traverse list function) -> list
;; Traverses sequences, both flat and recursive, applying
;; an arbitrary function to each one of its nodes.
;; (traverse '((1 2) 3 (4)) add1) -> '((2 3) 4 (5))

(define traverse
  (lambda (xs fn)
    (map (lambda (x)
           (if (pair? x)
               (traverse x fn)
               (fn x)))
         xs)))
