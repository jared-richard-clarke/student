;; Scheme implementation as defined in The Little Schemer.

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

;;   The applicative order Y combinator as defined by Douglas Crockford in JavaScript.
;;   
;;   function Y(le) {
;;       return (function (f) {
;;           return f(f);
;;       })(function (f) {
;;           return le(function (x) {
;;               return f(f)(x);
;;           });
;;       });
;;   }
;;
;;    const factorial = Y(function (fac) {
;;        return function (n) {
;;            return n <= 2 ? n : n * fac(n - 1);
;;        };
;;    });
