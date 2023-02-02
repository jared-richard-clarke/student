;; (take number (list any)) -> (list any)
;; Takes the first "n" elements of list "xs". Takes the
;; whole list if "n" is greater than the length of "xs".
;; (take 2 '(1 2 3)) -> '(1 2)

(define take
  (lambda (n xs)
    (if (or (<= n 0) (null? xs))
        '()
        (cons (car xs) (take (- n 1) (cdr xs))))))

;; (take-while function (list any)) -> (list any)
;; Returns the longest possible prefix list of list "xs"
;; that satisfy predicate "test".
;; (take-while odd? '(1 3 5 2 4)) -> '(1 3 5)

(define take-while
  (lambda (test xs)
    (if (or (null? xs) (not (test (car xs))))
        '()
        (cons (car xs) (take-while test (cdr xs))))))

;; (drop number (list any)) -> (list any)
;; Drops the first "n" elements of list "xs". Drops the
;; whole list if "n" is greater than the length of "xs".
;; (drop 2 '(1 2 3)) -> '(3)

(define drop
  (lambda (n xs)
    (if (or (<= n 0) (null? xs))
        xs
        (drop (- n 1) (cdr xs)))))

;; (drop-while function (list any)) -> (list any)
;; Returns the remaining list of list "xs" that don't
;; satisfy predicate "test".
;; (drop-while odd? '(1 3 5 2 4)) -> '(2 4)

(define drop-while
  (lambda (test xs)
    (if (or (null? xs) (not (test (car xs))))
        xs
        (drop-while test (cdr xs)))))

;; (split-at number (list any)) -> (list (list any) (list any))
;; Returns a list of two lists where the first list is "xs" prefix
;; of length "n" and the second list is the remainder.
;; (split-at 2 '(1 2 3 4)) -> '((1 2) (3 4))

(define split-at
  (lambda (n xs)
    (list (take n xs) (drop n xs))))
