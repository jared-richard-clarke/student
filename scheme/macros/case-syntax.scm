;; The Scheme Programming Language, R. Kent Dybvig
;; Chapter 8: Syntactic Extension

(define-syntax case
  (lambda (x)
    (syntax-case x ()
      [(_ e c1 c2 ...)
       #`(let ([t e])
           #,(let loop ([c1 #'c1] [cx #'(c2 ...)])
               (if (null? cx)
                   (syntax-case c1 (else)
                     [(else e1 e2 ...) #'(begin e1 e2 ...)]
                     [((k ...) e1 e2 ...)
                      #'(if (memv t '(k ...)) (begin e1 e2 ...))])
                   (syntax-case c1 ()
                     [((k ...) e1 e2 ...)
                      #`(if (memv t '(k ...))
                            (begin e1 e2 ...)
                            #,(loop (car cx) (cdr cx)))]))))])))

(let ([x 4] [y 5])
  (case (+ x y)
    [(1 3 5 7 9) 'odd]
    [(0 2 4 6 8) 'even]
    [else 'out-of-range]))

;; - expands ->

(let ([x 4] [y 5])
  (let ([t (+ x y)])
    (if (memv t '(1 3 5 7 9))
        (begin 'odd)
        (if (memv t '(0 2 4 6 8))
            (begin 'even)
            (begin 'out-of-range)))))
