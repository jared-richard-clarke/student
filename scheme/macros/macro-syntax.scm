;; Non-hygienic "macro" defined in terms of "syntax-case".
;; Example pulled from Chez Scheme by R. Kent Dybvig.

(define-syntax macro
  (lambda (x)
    (syntax-case x ()
      ((_ name fn)
       #'(define-syntax name
           (lambda (x)
             (syntax-case x ()
               [(key . body)
                (datum->syntax #'key (fn (syntax->datum x)))])))))))
