;; Abstract object with message passing as defined in 
;; The Scheme Programming Language, by R. Kent Dybvig.

(define-syntax define-object
  (syntax-rules ()
    [(_ (name . constructors)
        ((property value) ...)
        ((method function) ...))
     (define name
       (lambda constructors
         (let* ([property value] ...)
           (letrec ([method function] ...)
             (lambda (message . arguments)
               (case message
                 [(method) (apply method arguments)] ...
                 [else (error 'name "invalid input: "
                              (cons message arguments))]))))))]
    [(_ (name . constructors)
        ((method function) ...))
     (define-object (name . constructors)
       ()
       ((method function) ...))]))

(define-object (stack)
  [(type  'stack)
   (state '())]
  [(type?  (lambda () type))
   (empty? (lambda () (null? state)))
   (push!  (lambda xs (set! state (append xs state))))
   (peek   (lambda () (car state)))
   (pop!   (lambda ()
             (let ([item (car state)])
               (set! state (cdr state))
               item)))
   (clear! (lambda () (set! state '())))])
