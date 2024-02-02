(define match
  (lambda (xs) #t))

(define step
  (lambda (x f)
    (lambda (xs)
      (cond
        [(null? xs) #f]
        [(char=? x (car xs)) (f (cdr xs))]
        [else #f]))))

(define compile
  (lambda (pattern)
    (let* ([regex   (string->list pattern)]
           [machine (fold-right step match regex)])
      (lambda (text)
        (let ([feed (string->list text)])
          (machine feed))))))

(define match-regex (compile "regex"))
