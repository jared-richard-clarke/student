;; === Combinator Functions ===

;; forward pipe (/> == |>)
(define (/> x f) (f x))

;; reverse pipe (</ == <|)
(define (</ f x) (f x))

;; forward composition
(define (>> f g) (lambda (x) (g (f x))))

;; reverse composition
(define (<< g f) (lambda (x) (g (f x))))
