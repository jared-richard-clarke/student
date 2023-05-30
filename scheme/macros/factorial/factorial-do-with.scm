;; The `do` syntax as defined by R. Kent Dybvig for The Scheme Programming Language.

(define-syntax do
  (lambda (x)
    (syntax-case x ()
      [(_ (binding ...) (test res ...) expr ...)
       (with-syntax ([((var val update) ...)
                      (map (lambda (b)
                             (syntax-case b ()
                               [(var val) #'(var val var)]
                               [(var val update) #'(var val update)]))
                           #'(binding ...))])
         #'(let loop ([var val] ...)
             (if test
                 (begin (if #f #f) res ...)
                 (begin expr ... (loop update ...)))))])))

(define factorial
  (lambda (n)
    (do ([operand n (- operand 1)]
         [product 1 (* product operand)])
      ((zero? operand) product))))

;; === named let expansion ===

(define factorial-named-let
  (lambda (n)
    (let loop ([operand n] [product 1])
      (if (zero? operand)
          (begin (if #f #f) product)
          (begin (loop (- operand 1) (* product operand)))))))

;; "The odd-looking expression `(if #f #f)` is inserted before the result expressions `res ...`
;;  in case no result expressions are provided, since begin requires at least one subexpression.
;;  The value of `(if #f #f)` is unspecified, which is what we want since the value of do is
;;  unspecified if no result expressions are provided."
;;
;; — The Scheme Programming Language, 4th edition
