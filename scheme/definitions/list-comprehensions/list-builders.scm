;; (for-list expression [(x <- mx) ...] predicate?)
;; A simplified list comprehension pulled from the Haskell programming language.
;; === example ===
;; (define py-triple
;;   (lambda (n)
;;     (for-list (list x y z)
;;               [(x <- (range 1 n))
;;                (y <- (range x n))
;;                (z <- (range y n))]
;;               (= (+ (sqr x) (sqr y))
;;                  (sqr z)))))
;;
;; - expands ->
;; (define py-triple
;;   (lambda (n)
;;     (bind (range 1 n)
;;           (lambda (x)
;;             (bind (range x n)
;;                   (lambda (y)
;;                     (bind (range y n)
;;                           (lambda (z)
;;                             (if (= (+ (sqr x) (sqr y))
;;                                    (sqr z))
;;                                 (return (list x y z))
;;                                 empty)))))))))
;;
;; - so that ->
;; (py-triple 21)
;; - evaluates ->
;; '((3 4 5) (5 12 13) (6 8 10) (8 15 17) (9 12 15) (12 16 20))

(define-syntax for-list
  (syntax-rules (<-)
    [(_ expression [(x <- mx)])
     (bind mx (lambda (x)
                (return expression)))]
    [(_ expression [(x <- mx) (y <- my) ...])
     (bind mx (lambda (x)
                (for-list expression [(y <- my) ...])))]
    [(_ expression [(x <- mx)] predicate)
     (bind mx (lambda (x)
                (if predicate
                    (return expression)
                    empty)))]
    [(_ expression [(x <- mx) (y <- my) ...] predicate)
     (bind mx (lambda (x)
                (for-list expression [(y <- my) ...] predicate)))]))

;; === monad ===

(define return
  (lambda (x) (list x)))

(define bind
  (lambda (xs f)
    (concat (map f xs))))

(define empty '())

;; === utils ===

(define concat
  (lambda (xs)
    (fold-right append '() xs)))

;; === test utils ===

(define-syntax assert
  (lambda (stx)
    (syntax-case stx ()
      [(_ compare x y)
       (syntax (let ([computed-x x]
                     [computed-y y])
                 (unless (compare computed-x computed-y)
                   (printf "Test failed:\nlhs: ~a -> ~a, rhs: ~a -> ~a\n"
                           (quote x)
                           x
                           (quote y)
                           y))))])))

;; === monad laws ===

;; left identity: return x >>= f = f x

(let ([add1 (lambda (x) (list (add1 x)))])
  (assert equal? (bind (return 6) add1) (add1 6)))

;; right identity: m >>= return = m

(assert equal? (bind (list 7) return) (list 7))

;; associativity: (m >>= f) >>= g = m >>= (\x -> f x >>= g)

(let ([sub1 (lambda (x) (list (sub1 x)))]
      [add1 (lambda (x) (list (add1 x)))])
  (assert equal?
          (bind (bind (list 7) sub1) add1)
          (bind (list 7) (lambda (x) (bind (sub1 x) add1)))))
