; Definition according to "The Scheme Programming Language" by R. Kent Dybvig
(define map
  (lambda (func lst . more)
    (if (null? more)
        (let map1 ([lst lst])
          (if (null? lst)
              '()
              (cons (func (car lst))
                    (map1 (cdr lst)))))
        (let map-more ([lst lst] [more more])
          (if (null? lst)
              '()
              (cons
                (apply func (car lst) (map car more))
                (map-more (cdr lst) (map cdr more))))))))
