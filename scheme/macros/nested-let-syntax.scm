(define-syntax let*
  (lambda (x)
    (syntax-case x ()
      ;; === base case ===
      [(_ () b1 b2 ...)
       (syntax (let () b1 b2 ...))]
      ;; === recursive case ===
      [(_ ([i1 e1] [i2 e2] ...) b1 b2 ...)
       (syntax (let ([i1 e1])
                 (let* ([i2 e2] ...) b1 b2 ...)))])))

;; === nested let ===

(let* ([x 3]
       [y 4]
       [z (* x y)])
  z)

;; === let expansion ===

(let ([x 3])
  (let ([y 4])
    (let ([z (* x y)])
      (let ()
        z))))

;; === lambda expansion ===

((lambda (x)
   ((lambda (y)
      ((lambda (z)
         ((lambda () z)))
       (* x y)))
    4))
 3)
