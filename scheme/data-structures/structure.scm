;; The Scheme Programming Language, R. Kent Dybvig
;; Chapter 8: Syntactic Extension

;; The "define-structure" macro creates a custom data type
;; whose underlying implementation is a vector.
(define-syntax define-structure
  (lambda (x)
    (define gen-id
      (lambda (template-id . args)
        (datum->syntax template-id
                       (string->symbol
                         (apply string-append
                                (map (lambda (x)
                                       (if (string? x)
                                           x
                                           (symbol->string (syntax->datum x))))
                                     args))))))
    (syntax-case x ()
      [(_ name field ...)
       (with-syntax ([constructor (gen-id #'name "make-" #'name)]
                     [predicate (gen-id #'name #'name "?")]
                     [(access ...)
                      (map (lambda (x) (gen-id x #'name "-" x))
                           #'(field ...))]
                     [(assign ...)
                      (map (lambda (x)
                             (gen-id x "set-" #'name "-" x "!"))
                           #'(field ...))]
                     [structure-length (+ (length #'(field ...)) 1)]
                     [(index ...)
                      (let loop ([i 1] [ids #'(field ...)])
                        (if (null? ids)
                            '()
                            (cons i (loop (+ i 1) (cdr ids)))))])
         #'(begin
             (define constructor
               (lambda (field ...)
                 (vector 'name field ...)))
             (define predicate
               (lambda (x)
                 (and (vector? x)
                      (= (vector-length x) structure-length)
                      (eq? (vector-ref x 0) 'name))))
             (define access
               (lambda (x)
                 (vector-ref x index)))
             ...
             (define assign
               (lambda (x update)
                 (vector-set! x index update)))
             ...))])))

;; === Binary Tree ===
(define-structure binary-tree left right)

;; - expands ->

(begin
  (define make-binary-tree
    (lambda (left right)
      (vector 'binary-tree left right)))
 
  (define binary-tree?
    (lambda (x)
      (and (vector? x)
           (= (vector-length x) 3)
           (eq? (vector-ref x 0) 'binary-tree))))
 
  (define binary-tree-left
    (lambda (x) (vector-ref x 1)))
 
  (define binary-tree-right
    (lambda (x) (vector-ref x 2)))
 
  (define set-binary-tree-left!
    (lambda (x update)
      (vector-set! x 1 update)))
 
  (define set-binary-tree-right!
    (lambda (x update)
      (vector-set! x 2 update))))
