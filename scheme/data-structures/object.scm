;; Abstract object with message passing as defined in 
;; The Scheme Programming Language, by R. Kent Dybvig.

(define-syntax define-object
  (syntax-rules ()
    [(_ (name . initial-state)
        ((property value) ...)
        ((method function) ...))
     (define name
       (lambda initial-state
         (let* ([property value] ...)
           (letrec ([method function] ...)
             (lambda (message . arguments)
               (case message
                 [(method) (apply method arguments)] ...
                 [else
                  (error 'name "invalid input: "
                         (cons message arguments))]))))))]
    [(_ (name . initial-state)
        ((method function) ...))
     (define-object (name . initial-state)
       ()
       ((method function) ...))]))

(define-object (stack)
  [(name 'stack)
   (state '())]
  [(type?  (lambda () name))
   (empty? (lambda () (null? state)))
   (push!  (lambda xs (set! state (append xs state))))
   (peek   (lambda () (car state)))
   (pop!   (lambda ()
             (let ([item (car state)])
               (set! state (cdr state))
               item)))
   (clear! (lambda () (set! state '())))])
