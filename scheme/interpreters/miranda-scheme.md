# Miranda and Scheme Evaluators

**Why Calculating is Better than Scheming** by Philip Wadler

## Miranda

```
|| data types

term ::= Var var
       | Lambda var term
       | Apply term term
       | Closure env var term

env == [(var, term)]
var == [char]

|| evaluate and apply

eval e (Var v) = lookup e v
eval e (Lambda v t) = Closure e v t
eval e (Apply t1 t2) = apply (eval e t1) (eval e t2)

apply (Closure e v t1) t2 = eval (extend e v t2) t1

|| environment manipulation

lookup ((v1, t):e) v2 = t,           if (v1 = v2)
                      = lookup e v2, otherwise

extend e v t = (v, t):e
empty = []
```

## Scheme

```scheme
;; === evaluate term "t" in environment "e" ===

(define (eval e t)
  (cond [(variable? t)
         (lookup e (variable-name t))]
        [(lambda? t)
         (make-closure e (lambda-var t) (lambda-body t))]
        [(apply? t)
         (apply (eval e (apply-operator t))
                (eval e (apply-operand t)))]))

;; === apply term t1 to t2 ===

(define (apply t1 t2)
  (cond [(closure? t1)
         (eval (extend (closure-env t1) (closure-var t1) t2)
               (closure-body t1))]))

;; === environment manipulation ===

(define (lookup v e)
  (cond [(pair? e)
         (if (eq? v (caar e))
             (cadr e)
             (lookup v (cdr e)))]))

(define (extend e v t) (cons (cons v t) e))
(define empty nil?)

;; === create and access terms ===

;; variable term
(define (make-var v) v)
(define (variable? t) (atom? t))
(define (variable-name t) t)

;; lambda term
(define (make-lambda v t) (list 'lambda (list v) t))
(define (lambda? t)
  (and (not (atom? t))
       (eq? (car t) 'lambda)))
(define (lambda-var t) (caadr t))
(define (lambda-body t) (caddr t))

;; apply term
(define (make-apply t1 t2) (list t1 t2))
(define (apply? t)
  (and (not (atom? t))
       (not (eq? (car t) 'lambda))))
(define (apply-operator t) (car t))
(define (apply-operand t) (cadr t))

;; closure term
(define (make-closure e v t) (list 'closure e v t))
(define (closure? c)
  (and (not (atom? c))
       (eq? (car c) 'closure)))
(define (closure-env c) (cadr c))
(define (closure-var c) (caddr c))
(define (closure-body c) (cadddr c))
```
