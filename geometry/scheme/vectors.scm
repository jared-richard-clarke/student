;; Functional implementation of vectors and operations in linear vector space.

(library (vectors)
         (export vec2
                 vec3
                 vec4
                 vec2?
                 vec3?
                 vec4?
                 vec-add
                 vec-sub
                 vec-neg
                 vec-abs
                 vec-invert
                 vec-sum
                 vec-mag
                 vec-scale
                 vec-dot
                 vec-dist
                 vec-lerp
                 vec-normalize
                 vec-round)
         (import (rnrs base)
                 (rnrs lists)
                 (utils))

         ;; (vec2 number number) -> (vector number number)
         ;; Returns two-dimensional coordinates as a vector of two numbers.
         ;; (vec2 3 4) -> '#(3 4)

         (define (vec2 x y)
           (vector x y))

         ;; (vec3 number number number) -> (vector number number number)
         ;; Returns three-dimensional coordinates as a vector of three numbers.
         ;; (vec3 3 4 5) -> '#(3 4 5)

         (define (vec3 x y z)
           (vector x y z))

         ;; (vec4 number number number number) -> (vector number number number number)
         ;; Returns four-dimensional coordinates as a vector of three numbers.
         ;; (vec4 3 4 5 6) -> '#(3 4 5 6)

         (define (vec4 x y z w)
           (vector x y z w))

         ;; (vec-type number) -> (function vector) -> boolean
         ;; Creates functions that assert vector identity.
         ;; (define vec2? (vec-type 2)) -> (vec2? (vec2 1 2)) -> #t

         (define (vec-type dimensions)
           (lambda (v)
             (and (vector? v)
                  (= (vector-length v) dimensions))))

         ;; (vec2? any) -> boolean
         ;; Returns #t if value is a two-dimensional vector, #f otherwise.
         ;; (vec2? '#(3 4)) -> #t

         (define vec2? (vec-type 2))

         ;; (vec3? any) -> boolean
         ;; Returns #t if value is a three-dimensional vector, #f otherwise.
         ;; (vec3? '#(3 4 5)) -> #t

         (define vec3? (vec-type 3))

         ;; (vec4? any) -> boolean
         ;; Returns #t if value is a 4-dimensional vector, #f otherwise.
         ;; (vec4? '#(3 4 5 1)) -> #t
         (define vec4? (vec-type 4))

         ;; (binary function) -> (function vector vector) -> vector
         ;; Creates functions that perform binary operations over two vectors.
         ;; (define vec-add (binary +)) -> (vec-add (vec2 1 2) (vec2 3 4)) -> #(4 5)

         (define (binary operation)
           (lambda (v1 v2)
             (vector-map (lambda (x y) (operation x y)) 
                         v1 
                         v2)))

         ;; (vec-add (vector number number) (vector number number)) -> (vector number number)
         ;; Returns the sum of two vectors.
         ;; (vec-add (vec2 3 4) (vec2 7 11)) -> #(10 15)

         (define vec-add (binary +))

         ;; (vec-sub (vector number number) (vector number number)) -> (vector number number)
         ;; Returns the difference of two vectors.
         ;; (vec-sub (vec2 3 4) (vec2 7 11)) -> #(-4 -7)

         (define vec-sub (binary -))

         ;; (vec-neg vector) -> (- vector)
         ;; Flips the signs of the vector components.
         ;; (vec-neg (vec2 3 4)) -> (vec2 -3 -4)

         (define (vec-neg vec)
           (vector-map (lambda (x) (- x))
                       vec))

         ;; (vec-abs vector) -> vector
         ;; Returns vector with the absolute values of its vector components.
         ;; (vec-abs (vec2 -3 -4)) -> (vec2 3 4)

         (define (vec-abs vec)
           (vector-map (lambda (x) (abs x))
                       vec))

         ;; (vec-invert vector) -> vector
         ;; Inverts the vector components.
         ;; (vec-invert (vec2 2 2)) -> '#(0.5 0.5)

         (define (vec-invert vec)
           (vector-map (lambda (x) (/ 1 x))
                       vec))

         ;; (vec-sum vector ...) -> vector
         ;; Returns the sum of a series of vectors.
         ;; (vec-sum (vec2 1 2) (vec2 1 2) (vec2 3 4)) -> #(5 8)

         (define (vec-sum . vecs)
           (fold-left (lambda (accum vec)
                        (vector-map (lambda (x y) (+ x y)) accum vec))
                      (car vecs)
                      (cdr vecs)))

         ;; (vec-mag (vector number number)) -> number
         ;; Returns the magnitude of a vector.
         ;; (vec-mag (vec2 3 4)) -> 5

         (define (vec-mag vec)
           (apply hypotenuse (vector->list vec)))

         ;; (vec-scale vector number) -> vector
         ;; Returns a vector multiplied by a number.
         ;; (vec-scale (vec2 1 2) 2) -> #(2 4)

         (define (vec-scale vec scalar)
           (vector-map (lambda (x) (* scalar x)) vec))

         ;; (vec-dot (vector number number) (vector number number)) -> number
         ;; Returns the dot product of two vectors.
         ;; (vec-dot (vec2 1 2) (vec2 3 4)) -> 11

         (define (vec-dot v1 v2)
           (apply + (vector->list (vector-map (lambda (x y) (* x y))
                                              v1
                                              v2))))

         ;; (vec-dist (vector number ...) (vector number ...)) -> number
         ;; Returns the distance between two vectors.
         ;; (vec-dist (vec2 8 0) (vec2 1 0)) -> 7

         (define (vec-dist v1 v2)
           (apply hypotenuse (vector->list (vector-map (lambda (x y) (- y x))
                                                       v1
                                                       v2))))

         ;; (vec-lerp vector vector number) -> vector
         ;; Interpolates the components of two vectors.
         ;; (vec-lerp (vec2 1 1) (vec2 3 2) 0.5) -> (vec2 2 1.5)

         (define (vec-lerp v1 v2 t)
           (vector-map (lambda (x y) (+ x (* (- y x) t)))
                       v1
                       v2))

         ;; (vec-normalize (vector number ...)) -> (vector number ...)
         ;; Returns the unit vector of a vector.
         ;; (vec-normalize (vec2 3 4)) -> #(3/5 4/5)

         (define (vec-normalize vec)
           (let ([m (vec-mag vec)])
             (vector-map (lambda (x) (/ x m)) vec)))

         ;; (vec-round (vector number ...)) -> (vector number ...)
         ;; Rounds the vector components.
         ;; (vec-round (vec2 1.3 1.7)) -> #(1.0 2.0)

         (define (vec-round vec)
           (vector-map (lambda (x) (round x)) vec))

         )
