;; In order to use a recursive function with "let", I must pass the function to itself.
;; Normally, I would use either "letrec" or "letrec*" syntax for recursive functions.
;; I include this awkward example to help me better understand variable binding and
;; recursion in Scheme.

;; === let expression ===
(let ([sum (lambda (sum lst)
             (if (null? lst)
                 0
                 (+ (car lst)
                    (sum sum (cdr lst)))))])
  (sum sum '(1 2 3 4 5 6 7)))

;; === lambda expansion ===
((lambda (sum) (sum sum '(1 2 3 4 5 6 7)))
 (lambda (sum lst)
   (if (null? lst)
       0
       (+ (car lst)
          (sum sum (cdr lst))))))
