;; Syntactic form "cond" as defined in "The Scheme Programming Language" by R. Kent Dybvig.

(define-syntax cond
  (lambda (x)
    (syntax-case x ()
      [(_ c1 c2 ...)
       (let loop ([c1 (syntax c1)] [cx (syntax (c2 ...))])
         (if (null? cx)
             (syntax-case c1 (else =>)
               [(else e2 e3 ...)
                (syntax (begin e2 e3 ...))]
               [(e1)
                (syntax (let ([t e1]) (if t t)))]
               [(e1 => e2)
                (syntax (let ([t e1]) (if t (e2 t))))]
               [(e1 e2 e3 ...)
                (syntax (if e1 (begin e2 e3 ...)))])
             (with-syntax ([rest (loop (car cx) (cdr cx))])
               (syntax-case c1 (=>)
                 [(e1)
                  (syntax (let ([t e1]) (if t t rest)))]
                 [(e1 => e2)
                  (syntax (let ([t e1]) (if t (e2 t) rest)))]
                 [(e1 e2 e3 ...)
                  (syntax (if e1 (begin e2 e3 ...) rest))]))))])))

(define (traverse tree fn)
  (cond [(null? tree) '()]
        [(not (pair? tree)) (fn tree)]
        [else (cons (traverse (car tree) fn)
                    (traverse (cdr tree) fn))]))

;; - expands ->

(define (traverse tree fn)
  (if (null? tree)
      (begin '())
      (if (not (pair? tree))
          (begin (fn tree))
          (begin (cons (traverse (car tree) fn)
                       (traverse (cdr tree) fn))))))

(define (memoize fn)
  (define cache '())
  (lambda xs
    (cond
      [(assoc xs cache) => cdr]
      [else
       (let ([result (apply fn xs)])
         (set! cache (cons (cons xs result) cache))
         result)])))

;; - expands ->

(define (memoize fn)
  (define cache '())
  (lambda xs
    (let ((t (assoc xs cache)))
      (if t
          (cdr t)
          (begin (let ([result (apply fn xs)])
                   (set! cache (cons (cons xs result) cache))
                   result))))))
