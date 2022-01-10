; named let
(let range ([number 11]
            [lst '()])
  (if (= number 0)
      lst
      (range (- number 1) (cons number lst))))

; letrec expansion
((letrec ((range
           (lambda (number lst)
             (if (= number 0) lst (range (- number 1) (cons number lst))))))
   range)
 11
 '())
; let expansion
((let ((range #f))
   (set! range
         (lambda (number lst) (if (= number 0) lst (range (- number 1) (cons number lst)))))
   (let () range))
 11
 '())
; lambda expansion
(((lambda (range)
    (set! range
          (lambda (number lst) (if (= number 0) lst (range (- number 1) (cons number lst)))))
    ((lambda () range)))
  #f)
 11
 '())
