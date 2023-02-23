(define-syntax for-list
  (syntax-rules (<-)
    [(_ expression [(x <- mx)])
     (bind mx (lambda (x)
                (return expression)))]
    [(_ expression [(x <- mx) (y <- my) ...])
     (bind mx (lambda (x)
                (for-list expression [(y <- my) ...])))]
    [(_ expression [(x <- mx)] predicate)
     (bind mx (lambda (x)
                (if predicate
                    (return expression)
                    (return))))]
    [(_ expression [(x <- mx) (y <- my) ...] predicate)
     (bind mx (lambda (x)
                (for-list expression [(y <- my) ...] predicate)))]))

(define return
  (case-lambda [(x) (list x)]
               [()  (list)]))

(define bind
  (lambda (xs f)
    (concat (map f xs))))

(define concat
  (lambda (xs)
    (fold-right append '() xs)))

;; (for-list x [(x <- '(1 2 3 4 5 6 7 8 9 10))] (even? x))
;;
;; evaluates ->
;;
;; '(2 4 6 8 10)
