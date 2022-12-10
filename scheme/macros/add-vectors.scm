(define (add v1 v2)
  (let* ([r vector-ref]
         [x1 (r v1 0)]
         [y1 (r v1 1)]
         [x2 (r v2 0)]
         [y2 (r v2 1)])
    (vector (+ x1 x2) (+ y1 y2))))

;; === let expansion ===

(define add-let
  (lambda (v1 v2)
    (let ([r vector-ref])
      (let ([x1 (r v1 0)])
        (let ([y1 (r v1 1)])
          (let ([x2 (r v2 0)])
            (let ([y2 (r v2 1)])
              (let ()
                (vector (+ x1 x2) (+ y1 y2))))))))))

;; === lambda expansion ===

(define add-lambda
  (lambda (v1 v2)
    ((lambda (r)
       ((lambda (x1)
          ((lambda (y1)
             ((lambda (x2)
                ((lambda (y2)
                   ((lambda ()
                      (vector (+ x1 x2) (+ y1 y2)))))
                 (r v2 1)))
              (r v2 0)))
           (r v1 1)))
        (r v1 0)))
     vector-ref)))
