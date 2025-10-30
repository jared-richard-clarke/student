;; In order to use a recursive function with "let", I must pass the function to itself.
;; Normally, I would use either "letrec" or "letrec*" syntax for recursive functions.
;; I include this awkward example to help me better understand variable binding and
;; recursion in Scheme.

;; === let expression ===
(let ([sum (lambda (sum xs)
             (if (null? xs)
                 0
                 (+ (car xs)
                    (sum sum (cdr xs)))))])
  (sum sum '(1 2 3 4 5 6 7)))

;; === lambda expansion ===
((lambda (sum) (sum sum '(1 2 3 4 5 6 7)))
 (lambda (sum xs)
   (if (null? xs)
       0
       (+ (car xs)
          (sum sum (cdr xs))))))

;; === more generally ===
;; Z = λf.(λg.f (λx.(g g) x)) (λg.f (λx.(g g) x))
;;
;; Z is the strict form of the Y combinator, in that
;; function application (g g) is wrapped in a thunk
;; to prevent infinite recursion.
(define Z
  (lambda (f)
    ((lambda (g)
       (f (lambda (x) ((g g) x))))
     (lambda (g)
       (f (lambda (x) ((g g) x)))))))

(define summation
  (Z (lambda (sum)
       (lambda (xs)
         (if (null? xs)
             0
             (+ (car xs)
                (sum (cdr xs))))))))
