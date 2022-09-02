;; "and" and "or" syntax as defined in the Revised7 Report on the Algorithmic Language Scheme

(define-syntax and
  (syntax-rules ()
    [(_) #t]
    [(_ e) e]
    [(_ e1 e2 e3 ...)
     (if e1 (and e2 e3 ...) #f)]))

(define-syntax or
  (syntax-rules ()
    [(_) #f]
    [(_ e) e]
    [(_ e1 e2 e3 ...)
     (let ([t e1])
       (if t t (or e2 e3 ...)))]))

;; "or" defined using "syntax" and "syntax-case" in place of "syntax-rules".

(define-syntax or
  (lambda (x)
    (syntax-case x ()
      [(_) #'#f]
      [(_ e) #'e]
      [(_ e1 e2 e3 ...)
       #'(let ([t e1]) (if t t (or e2 e3 ...)))])))

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
