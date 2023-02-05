;; (append (list any) ... any)
;; Combines a variable number of lists. The final argument need not be a list.
;; (append '(1 2 3) '())  -> '(1 2 3)
;; (append '(1 2) '(3 4) '(5 6 7)) -> '(1 2 3 4 5 6 7)

(define append
  (lambda args
    (let loop-1 ([lst '()]
                 [args args])
      (if (null? args)
          lst
          (let loop-2 ([lst lst])
            (if (null? lst)
                (loop-1 (car args)
                        (cdr args))
                (cons (car lst)
                      (loop-2 (cdr lst)))))))))

;; === alternatives ===

;; (append (list any) (list any)) -> (list any)
;; Combines two lists.
;; (append '(1 2) '(3 4)) -> '(1 2 3 4)

(define append
  (lambda (xs ys)
    (if (empty? ys)
        xs
        (fold-right cons ys xs))))

;; (concat (list any) ...) -> (list any)
;; Combines variable number of lists.
;; (concat '(1 2) '(3 4) '(5 6 7)) -> '(1 2 3 4 5 6 7)

(define concat
  (lambda xs
    (fold-right append '() xs)))
