;; "and" and "or" syntax as defined in the Revised7 Report on the Algorithmic Language Scheme

(define-syntax and
  (lambda (stx)
    (syntax-case stx ()
      [(_)   (syntax #t)]
      [(_ x) (syntax x)]
      [(_ x y z ...)
       (syntax (if x (and y z ...) #f))])))

(define-syntax or
  (lambda (stx)
    (syntax-case stx ()
      [(_)   (syntax #f)]
      [(_ x) (syntax x)]
      [(_ x y z ...)
       (syntax (let ([t x]) 
                 (if t t (or y z ...))))])))

;; === De Morgan's Laws ===

;; 1. The negation of a disjunction is the conjunction of the negations
;; not (A or B) = (not A) and (not B)

;; 2. The negation of a conjunction is the disjunction of the negations
;; not (A and B) = (not A) or (not B)

(define A #t)
(define B #t)

;; not (A or B) = (not A) and (not B) -> #t

(eq? (not (or A B))
     (and (not A) (not B)))

;; macro expansion

(eq? (not (let ([t A])
            (if t 
                t 
                B)))
     (if (not A)
         (not B)
         #f))

;; ========================================

;; not (A and B) = (not A) or (not B) -> #t

(eq? (not (and A B))
     (or (not A) (not B)))

;; macro expansion

(eq? (not (if A
              B
              #f))
     (let ([t (not A)])
       (if t
           t
           (not B))))
