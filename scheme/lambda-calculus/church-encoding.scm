;; Church Encoding
;; reimplementation of the cons cell using only functions

(define (cons x y)
  (lambda (message) (message x y)))

(define (car cons-cell)
  (cons-cell (lambda (head tail) head)))

(define (cdr cons-cell)
  (cons-cell (lambda (head tail) tail)))

;; === expanded forms ===
;; (define cons
;;   (lambda (x y)
;;     (lambda (message) (message x y))))

;; (define car
;;   (lambda (cons-cell)
;;     (cons-cell (lambda (head tail) head))))

;; (define cdr
;;   (lambda (cons-cell)
;;     (cons-cell (lambda (head tail) tail))))
