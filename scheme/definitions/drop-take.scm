;; (drop number (list any)) -> (list any)
;; Drops the first "n" elements of list "xs". Evaluates to
;; an empty list if "n" is greater than the length of "xs".
;; (drop 2 '(1 2 3)) -> '(3)

(define drop
  (lambda (n xs)
    (if (or (<= n 0) (null? xs))
         xs
         (drop (- n 1) (cdr xs)))))

;; (take number (list any)) -> (list any)
;; Takes the first "n" elements of list "xs". Takes the
;; whole list if "n" is greater than the length of "xs".
;; (take 2 '(1 2 3)) -> '(1 2)

(define take
  (lambda (n xs)
    (if (or (<= n 0) (null? xs))
        '()
        (cons (car xs) (take (- n 1) (cdr xs))))))
