;; (take number (list any)) -> (list any)
;; Takes the first "n" elements of list "xs". Takes the
;; whole list if "n" is greater than the length of "xs".
;; (take 2 '(1 2 3)) -> '(1 2)

(define take
  (lambda (n xs)
    (if (or (<= n 0) (null? xs))
        '()
        (cons (car xs) (take (- n 1) (cdr xs))))))

;; (drop number (list any)) -> (list any)
;; Drops the first "n" elements of list "xs". Drops the
;; whole list if "n" is greater than the length of "xs".
;; (drop 2 '(1 2 3)) -> '(3)

(define drop
  (lambda (n xs)
    (if (or (<= n 0) (null? xs))
        xs
        (drop (- n 1) (cdr xs)))))

;; (split-at number (list any)) -> (list (list any) (list any))
;; Returns a list of two lists where the first list is "xs" prefix
;; of length "n" and the second list is the remainder.
;; (split-at 2 '(1 2 3 4)) -> '((1 2) (3 4))

(define split-at
  (lambda (n xs)
    (list (take n xs) (drop n xs))))
