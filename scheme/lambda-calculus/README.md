# The Lambda Calculus
I explore examples from the lambda calculus to better understand Scheme.

## Rules

The lambda calculus, in its simplest form, builds terms using the following rules:

| syntax   | name        | description |
| -------- | ----------- | ----------- |
| *x*      | variable    | A character or string representing a parameter, mathematical, or logical value.            |
| (λ*x.M*) | abstraction | Function definition *M* — a lambda term. The variable *x* becomes bound in the expression. |
| (*M N*)  | application | Applying a function to an argument. *M* and *N* are lambda terms.                          |

```
λx.x + 1

=== translates ===>

λ                x  .    +   x    7
^                ^  ^    ^   ^    ^
|                |  |    |   |    |
That function of x that adds x to 7.
```

## Bound and Free Variables

### Example 1
```
(λx.+ x y) 1
  ^     ^
  |     |
bound  free
```

### Example 2

```
+ x ((λx.+ x 6) 1)
  ^    ^
  |    |
free bound
```

## Scheme Lambda Expression
Lambda expression grammar as defined in [The Essentials of Programming Languages](https://en.wikipedia.org/wiki/Essentials_of_Programming_Languages) by Daniel P. Friedman and Mitchell Wand. This grammar defines lambda expressions as Scheme values.

The lambda calculus is a language that consists only of variable references, single-argument procedures, and procedure calls.

```
LcExp ::= Identifier
      ::= (lambda (Identifier) LcExp)
      ::= (LcExp LcExp)
where an identifier is any symbol other than lambda.
```
