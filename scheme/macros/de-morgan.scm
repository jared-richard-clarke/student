;; Syntax as defined in the Revised7 Report on the Algorithmic Language Scheme
(define-syntax and
  (syntax-rules ()
    [(and) #t]
    [(and test) test]
    [(and test1 test2 ...)
     (if test1 (and test2 ...) #f)]))

(define-syntax or
  (syntax-rules ()
    [(or) #f]
    [(or test) test]
    [(or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...)))]))

;; === De Morgan's Laws ===
;; 1. The negation of a disjunction is the conjunction of the negations
;; not (A or B) = (not A) and (not B)

;; 2. The negation of a conjunction is the disjunction of the negations
;; not (A and B) = (not A) or (not B)

(define-values (A B) (values #t #t))

;; 1. not (A or B) = (not A) and (not B) -> #t
(eq? (not (or A B))
     (and (not A) (not B)))

;; 1. macro expansion
(eq? (not (let ([x A])
            (if x x B)))
     (if (not A)
         (not B)
         #f))

;; 2. not (A and B) = (not A) or (not B) -> #t
(eq? (not (and A B))
     (or (not A) (not B)))

;; 2. macro expansion
(eq? (not (if A
              B
              #f))
     (let ([x (not A)])
       (if x
           x
           (not B))))
