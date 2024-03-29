# Scheme: Notes

## Types of Recursion

1. tail recursion
2. stack recursion
3. filter recursion
4. accumulative recursion
5. mutual recursion

## Scope

**Lexical scoping** means a function's scope does not inherit from its caller's scope,
but from the scope in which the function was defined. 

> "A variable usage refers to the preceding declaration with the same name in the 
> innermost scope that encloses the expression where the variable is used."
>  ... 
> "\[Closures\] should capture a frozen snapshot of the environment as 
> it existed at the moment the function was declared."
>
> — [Crafting Interpreters](https://craftinginterpreters.com/resolving-and-binding.html) by Robert Nystrom

Examples pulled from [Chapter 3.4.4 Lexical Scope](https://www.gnu.org/software/guile/manual/guile.html#About-Closure) 
of the Guile Reference Manual

> “Lexical scope” in general is the idea that
>
> - an identifier at a particular place in a program always refers to the same variable location — 
>   where “always” means “every time that the containing expression is executed”, and that
> - the variable location to which it refers can be determined by static examination of the source code 
>   context in which that identifier appears, without having to consider the flow of execution through the program as a whole.
What does the identifier `currency-abbreviation` refer to in the `currency-string` function?

### Dynamic Scope ( Emacs Lisp )

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

### Lexical Scope ( Scheme )

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

---

## Closure

- When the Scheme interpreter evaluates a `lambda` expression, it stores the current environment as part of the procedure definition.
- Whenever that procedure is called, the interpreter reinstates the environment that is stored in 
  the procedure definition and evaluates the procedure body within the context of that environment.
  
> "A closure is an object that combines the function with the lexical bindings of its free variables at the time it is created.
>  Closures are first class data objects because they may be passed as arguments to or returned as values from other functions,
>  or stored in the system indefinitely."
>
> — **Three Implementation Models for Scheme** by R. Kent Dybvig

---

## Special Forms

> "When a procedure invocation expression is evaluated, the procedure and all 
> the argument expressions must be evaluated before the procedure can be invoked. 
> Special syntactic expressions are special because they are able to manipulate their 
> arguments in an unevaluated form, and can choose whether to evaluate any or all 
> of the argument expressions." 
>
> [Guile Manual](https://www.gnu.org/software/guile/manual/guile.html#Introduction)

---

## Tail Calls
A call is in tail position with respect to a function if its value is returned directly. 
Nothing is left to do after the call but to return from the function.

`func-x` is in the tail position, whereas `func-y` is not.

```scheme
(lambda () (func-x (func-y)))
(lambda () (if (func-y) (func-x) (func-x)))
(lambda () (let [[x 7]] (func-x)))
(lambda () (or (func-y) (func-x)))
```
