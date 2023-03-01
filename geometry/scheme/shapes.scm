(library (shapes)
         (export rectangle
                 right-triangle
                 circle)
         (import (rnrs))
         
         (define-record-type rectangle
           (fields length
                   width
                   area
                   perimeter
                   diagonal)
           (protocol
            (lambda (new)
              (lambda (l w)
                (let ([area (* l w)]
                      [perimeter (* (+ l w) 2)]
                      [diagonal (hypotenuse l w)])
                  (new l w area perimeter diagonal))))))

         (define-record-type right-triangle
           (fields base
                   height
                   area
                   perimeter
                   hypotenuse)
           (protocol
            (lambda (new)
              (lambda (b h)
                (let ([area (* 1/2 b h)]
                      [hypot (hypotenuse b h)])
                  (new b h area (+ b h hypot) hypot))))))

         (define-record-type circle
           (fields radius
                   diameter
                   area
                   circumference)
           (protocol
            (lambda (new)
              (lambda (r)
                (let ([area (* 2 π r)]
                      [circumference (* π (sqr r))])
                  (new r (* r 2) area circumference)))))) 

         )
