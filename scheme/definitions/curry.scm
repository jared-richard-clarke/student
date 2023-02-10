;; Transforms a multi-parameter function into a sequence of functions
;; wherein each argument is called with a single function.
;; (define add (curry (x y) (+ x y))
;; ->
;; (define add
;;   (lambda (x)
;;     (lambda (y) (+ x y))))

(define-syntax curry
  (lambda (stx)
    (syntax-case stx ()
      [(_ (x) y z ...)
       (let ([id (syntax x)])
         (unless (identifier? id)
           (syntax-violation 'argument "non-identifier" id)))
       (syntax (lambda (x) y z ...))]
      [(_ (x y ...) z w ...)
       (syntax (lambda (x)
                 (curry (y ...) z w ...)))])))
