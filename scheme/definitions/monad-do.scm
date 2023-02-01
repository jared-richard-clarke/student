;; The Haskell "do" syntax (simplified). Makes monads readable.
;; "bind" must be defined in order for this macro to work.
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

(define-syntax monad-do
  (lambda (stx)
    (syntax-case stx (<-)
      [(_ expression)
       (syntax expression)]
      [(_ (x <- mx) expression ...)
       (syntax (bind mx (lambda (x) 
                          (monad-do expression ...))))])))
