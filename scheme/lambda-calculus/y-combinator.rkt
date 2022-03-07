#lang racket
#|
    The applicative-order Y combinator

    Y = λƒ. (λx.ƒ (x x)) (λx.ƒ (x x))
    
    ----------------------------------
    
    The applicative order Y combinator as defined by Douglas Crockford in JavaScript.
    
    function Y(le) {
        return (function (f) {
            return f(f);
        })(function (f) {
            return le(function (x) {
                return f(f)(x);
            });
        });
    }

    const factorial = Y(function (fac) {
        return function (n) {
            return n <= 2 ? n : n * fac(n - 1);
        };
    });
|#

(define Y-scm
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

(define Y
  (λ (le)
    ((λ (ƒ) (ƒ ƒ))
     (λ (ƒ)
       (le (λ (x) ((ƒ ƒ) x)))))))
