;; (head list) -> any
;; Returns the first element in a list.
;; (head '(1 2 3)) -> 1

(define (head lst)
  (apply (lambda (x . y) x) lst))

;; (tail list) -> list
;; Returns the input list minus its first element.
;; (tail '(1 2 3)) -> '(2 3)

(define (tail lst)
  (apply (lambda (x . y) y) lst))

;; (head-tail list) -> any list
;; Returns both the head and tail of a list.
;; (head-tail '(1 2 3)) -> 1 '(2 3)

(define (head-tail lst)
  (values (head lst) (tail lst)))
