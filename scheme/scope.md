# Scope

Examples pulled from [Chapter 3.4.4 Lexical Scope](https://www.gnu.org/software/guile/manual/guile.html#About-Closure) 
of the Guile Reference Manual

> “Lexical scope” in general is the idea that
>
> - an identifier at a particular place in a program always refers to the same variable location — 
>   where “always” means “every time that the containing expression is executed”, and that
> - the variable location to which it refers can be determined by static examination of the source code 
>   context in which that identifier appears, without having to consider the flow of execution through the program as a whole.

What does the identifier `currency-abbreviation` refer to in the `currency-string` function?

## Dynamic Scope ( Emacs Lisp )

In Emacs Lisp, all variable bindings go onto a single stack. `currency-abbreviation` refers to the 
topmost binding from that stack, which has the name `currency-abbreviation`. 
The code does not create any local binding for the identifier currency-abbreviation, so all occurrences 
of this identifier refer to the top level variable.

```el
(defvar currency-abbreviation "USD")

(defun currency-string (units hundredths)
  (concat currency-abbreviation
          (number-to-string units)
          "."
          (number-to-string hundredths)))

(defun french-currency-string (units hundredths)
  (let ((currency-abbreviation "FRF"))
    (currency-string units hundredths)))
    
;; (french-currency-string 33 44) -> "FRF33.44"
```

## Lexical Scope ( Scheme )

The variable binding that Scheme creates for `currency-abbreviation` is purely local to the code 
that forms the body of the `let` expression. Since this code doesn’t directly use the name `currency-abbreviation` 
at all, the binding is pointless.

```scheme
(define currency-abbreviation "USD")

(define (currency-string units hundredths)
  (string-append currency-abbreviation
                 (number->string units)
                 "."
                 (number->string hundredths)))

(define (french-currency-string units hundredths)
  (let ((currency-abbreviation "FRF"))
    (currency-string units hundredths)))
    
;; (french-currency-string 33 44) -> "USD33.44"
```
