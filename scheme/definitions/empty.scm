;; The empty list. Conventionally called "null"
;; by many Scheme implementations.

(define empty '())
(define null empty)

;; (empty? any) -> boolean
;; If the argument is an empty list, return #t.
;; Otherwise return #f.

(define empty?
   (lambda (x)
      (eq? x '())))

(define null? empty?)
