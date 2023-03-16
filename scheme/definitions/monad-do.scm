;; The Haskell "do" syntax (simplified). Makes monads readable.
;; "bind" or ">>=", "return", and their semantics are defined separately.
;;
;; The syntax for the "then" or ">>" operator is omitted for simplicity.

(define-syntax monad-do
  (syntax-rules (<-)
    [(_ expression) expression]
    [(_ (x <- mx) expression ...)
     (bind mx (lambda (x) 
                (monad-do expression ...)))]))

;; === EXAMPLE ===
;;
;; (define and-then
;;   (lambda (px py)
;;     (monad-do (x <- px)
;;               (y <- py)
;;               (return (cons x y)))))
;;
;; expands ->
;;
;; (define and-then
;;   (lambda (px py)
;;     (bind px (lambda (x)
;;                (bind py (lambda (y)
;;                           (return (cons x y))))))))
