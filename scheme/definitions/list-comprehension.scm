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

;; === example 1 ===
;;
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
;;
;; === example 2 ===
;;
;; (for-list (cons x y) [(x <- '(a b)) (y <- '(1 2))])
;;
;; expands ->
;;
;; (bind '(a b) (lambda (x)
;;                (bind '(1 2) (lambda (y)
;;                               (return (cons x y))))))
;;
;; evaluates ->
;;
;; '((a . 1) (a . 2) (b . 1) (b . 2))
