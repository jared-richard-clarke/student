;; === Macro as defined in R6RS ===
(define-syntax letrec
  (lambda (x)
    (syntax-case x ()
      [(_ ((i e) ...) x y ...)
       (with-syntax ([(t ...) (generate-temporaries (syntax (i ...)))])
         (syntax (let ([i #f] ...)
                   (let ([t e] ...)
                     (set! i t)
                     ...
                     (let () x y ...)))))])))

;; === letrec expression ===
(letrec ([is-even? (lambda (n)
                     (or (zero? n)
                         (is-odd? (sub1 n))))]
         [is-odd? (lambda (n)
                    (and (not (zero? n))
                         (is-even? (sub1 n))))])
  (is-odd? 11))

;; === let expansion ===
(let ([is-even? #f]
      [is-odd? #f])
  (let ([is-even?-1 (lambda (n)
                      (or (zero? n) (is-odd? (sub1 n))))]
        [is-odd?-2  (lambda (n)
                      (and (not (zero? n)) (is-even? (sub1 n))))])
    (set! is-even? is-even?-1)
    (set! is-odd? is-odd?-2)
    (let () (is-odd? 11))))

;; === lambda expansion ===
((lambda (is-even? is-odd?)
   ((lambda (is-even?-1 is-odd?-2)
      (set! is-even? is-even?-1)
      (set! is-odd? is-odd?-2)
      ((lambda () (is-odd? 11))))
    (lambda (n)
      (or (zero? n) (is-odd? (sub1 n))))
    (lambda (n)
      (and (not (zero? n)) (is-even? (sub1 n))))))
 #f
 #f)
