;; (append (list any) ...) -> (list any)
;; Combines elements of multiple lists into a single list.
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

;; === alternative ===

;; (append (list any) ...) -> (list any)
;; Combines elements of multiple lists into a single list.
;; (append '(1 2) '(3 4) '(5 6 7)) -> '(1 2 3 4 5 6 7)

(define append
  (lambda xys
    (let ([++ (lambda (xs ys)
                (if (null? ys)
                    xs
                    (fold-right cons ys xs)))])
      (fold-right ++ '() xys))))
