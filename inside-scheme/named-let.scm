; named let
(let range ([number 11]
            [lst '()])
  (if (= number 0)
      lst
      (range (- number 1) (cons number lst))))

; expanded to letrec
((letrec ([range (lambda (number lst)
                   (if (= number 0)
                       lst
                       (range (- number 1) (cons number lst))))])
   range)
 11 '())

; expanded to nested let expressions
(let ([number 11]
      [lst '()])
  (let ([range #f])
    (let ([temp (lambda (number lst)
                  (if (= number 0)
                      lst
                      (range (- number 1) (cons number lst))))])
      (set! range temp)
      (let ()
        (range number lst)))))

; expanded to lambda expressions
((lambda (number lst)
   ((lambda (range)
      ((lambda (temp)
         (set! range temp)
         ((lambda ()
            (range number lst))))
       (lambda (number lst)
         (if (= number 0)
             lst
             (range (- number 1) (cons number lst))))))
    #f))
 11 '())
