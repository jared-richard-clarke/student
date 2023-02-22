;; (for-list (+ x y) [(x <- '(1 2 3)) (y <- '(4 5 6))])
;;
;; expands ->
;;
;; (bind '(1 2 3) (lambda (x)
;;                  (bind '(4 5 6) (lambda (y)
;;                                   (return (+ x y))))))
;;
;; returns ->
;;
;; '(5 6 7 6 7 8 7 8 9)

(define-syntax for-list
  (syntax-rules (<-)
    [(_ expression [(x <- mx)])
     (bind mx (lambda (x)
                (return expression)))]
    [(_ expression [(x <- mx) (y <- my) ...])
     (bind mx (lambda (x)
                (for-list expression [(y <- my) ...])))]))

(define return
  (lambda (x) (list x)))

(define bind
  (lambda (xs f)
    (concat (map f xs))))

(define concat
  (lambda (xs)
    (fold-right append '() xs)))
