;; The Haskell "do" syntax (simplified). Makes monads readable.
;; "bind" must be defined in order for this macro to work.
;;
;; (define and-then
;;   (lambda (px py)
;;     (do (x <- px)
;;         (y <- py)
;;        (return (cons x y)))))
;;
;; expands ->
;;
;; (define and-then
;;   (lambda (px py)
;;     (bind px (lambda (x)
;;                (bind py (lambda (y)
;;                           (return (cons x y))))))))

(define-syntax do
  (lambda (stx)
    (syntax-case stx (<-)
      [(_ expression)
       (syntax expression)]
      [(_ (px) expression ...)
       (syntax (bind px (lambda ()
                          (do expression ...))))]
      [(_ (x <- px) expression ...)
       (syntax (bind px (lambda (x) 
                          (do expression ...))))])))
