;; (rec var expr) -> value
;; Creates a recursive expression from "expr" by binding
;; "var" within "expr" to the value of "expr".

(define-syntax rec
  (syntax-rules ()
    [(_ x e)
     (letrec ([x e]) x)]))

;; === example ===

(map (rec sum
       (lambda (x)
         (if (= x 0)
             0
             (+ x (sum (- x 1))))))
     '(0 1 2 3 4 5))

;; === letrec expansion ===

(map (letrec ([sum (lambda (x)
                     (if (= x 0)
                         0
                         (+ x (sum (- x 1)))))]) sum)
     '(0 1 2 3 4 5))

;; === let expansion ===

(map (let ([sum #f])
       (let ([temp (lambda (x)
                     (if (= x 0)
                         0
                         (+ x (sum (- x 1)))))])
         (set! sum temp) sum))
     '(0 1 2 3 4 5))

;; === lambda expansion ===

(map ((lambda (sum)
        ((lambda (temp)
           (set! sum temp) sum)
         (lambda (x)
           (if (= x 0)
               0
               (+ x (sum (- x 1))))))) #f)
     '(0 1 2 3 4 5))
