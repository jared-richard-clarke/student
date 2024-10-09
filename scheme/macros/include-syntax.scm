;; (include string) -> (begin expression ...)
;; Expands into a "begin" expression containing the forms found in the file named by "string".
;; Defined according to the "Chez Scheme User Guide" by R. Kent Dybvig.

;; The "include" form uses "datum->syntax" to convert the objects read from the file
;; into syntax objects in the proper lexical context, so that identifier references
;; and definitions within those expressions are scoped to where the "include"
;; form appears.

(define-syntax include
  (lambda (x)
    (define read-file
      (lambda (fn k)
        (let ([p (open-input-file fn)])
          (let recur ([x (read p)])
            (if (eof-object? x)
                (begin (close-port p) '())
                (cons (datum->syntax k x) (recur (read p))))))))
    (syntax-case x ()
      [(k filename)
       (let ([fn (syntax->datum (syntax filename))])
         (with-syntax ([(expr ...) (read-file fn (syntax k))])
           (syntax (begin expr ...))))])))
