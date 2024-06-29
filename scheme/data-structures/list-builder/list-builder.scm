(library (list-builder)
         (export for <- when)
         (import (rnrs base)
                 (rnrs lists))

         (define-syntax for
           (syntax-rules (<- when)
             ;; base case
             [(_ ([x <- mx]) expression)
              (concat-map mx (lambda (x)
                               (list expression)))]
             ;; recursive case
             [(_ ([x <- mx] [y <- my] ...) expression)
              (concat-map mx (lambda (x)
                               (for ([y <- my] ...) expression)))]
             ;; base case with predicate
             [(_ ([x <- mx]) (when predicate) expression)
              (concat-map mx (lambda (x)
                               (if predicate
                                   (list expression)
                                   empty)))]
             ;; recursive case with predicate
             [(_ ([x <- mx] [y <- my] ...) (when predicate) expression)
              (concat-map mx (lambda (x)
                               (for ([y <- my] ...) (when predicate) expression)))]))

         ;; === monad ===

         (define empty '())

         ;; (define concat
         ;;   (lambda (xs)
         ;;     (fold-right append empty xs)))
         ;;
         ;; Calling map between concatenations produces intermediary lists.
         ;; Avoid extra allocations by integrating map directly into concat.
         ;;
         ;; (define concat-map
         ;;   (lambda (xs f)
         ;;     (concat (map f xs))))

         (define concat-map
           (lambda (xs f)
             (if (null? xs)
                 empty
                 (append (f (car xs)) (concat-map (cdr xs) f)))))

         )
