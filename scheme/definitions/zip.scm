;; (zip-with function list list ...) -> list
;; Combines lists pairwise, applying a function to each element pair.
;; Returns when shortest list is exhausted.
;; (zip-with + '(1 2 3) '(4 5 6) '(7 8 9)) -> '(12 15 18)

(define zip-with
  (lambda (fn . lists)
    (apply map fn lists)))

;; (zip list list ...) -> list
;; Combines lists pairwise. Returns when the shortest list is exhausted.
;; (zip '(1 2 3) '(4 5 6) '(7 8 9)) -> '((1 4 7) (2 5 8) (3 6 9))

(define zip
  (lambda lists
    (apply zip-with list lists)))
