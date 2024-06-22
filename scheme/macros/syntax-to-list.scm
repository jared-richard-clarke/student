;; === Chez Scheme Manual, Version 10.0 ===
;; Converts a syntax list into a list of syntax objects.

(define syntax->list
  (lambda (stx)
    (syntax-case stx ()
      [() (quote ())]
      [(x . xs) (cons (syntax x) (syntax->list (syntax xs)))])))

;; "syntax->list" is not required for list structures constructed
;; from individual pattern variable values or sequences of
;; pattern-variable values, since such structures are already lists.
;;
;; (list? (with-syntax ([x #'a] [y #'b] [z #'c]) #'(x y z))) -> #t
