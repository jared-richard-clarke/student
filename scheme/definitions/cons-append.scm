;; (::) and (@) notation derived from OCaml and its ML relatives.

;; (:: any any) -> pair
;; Alias for cons, which creates a LISP pair from two values.
;; (:: 1 '(2 3)) -> '(1 2 3)
;; (:: 'x 'y) -> '(x . y)

(define (:: head tail)
  (cons head tail))

;; (@ list ...) -> list | (@ list ... value) -> any
;; Alias for append, which – given all list arguments – returns a combined list.
;; If last argument is not a list, returns an improper list.
;; (@ '(1 2) '(3 4)) -> '(1 2 3 4)
;; (@ '(1 2) 3) -> '(1 2 . 3)

(define (@ . args)
  (apply append args))
