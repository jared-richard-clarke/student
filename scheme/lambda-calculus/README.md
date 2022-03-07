# The Lambda Calculus
I explore examples from the lambda calculus to better understand Scheme.

## Rules

The lambda calculus, in its simplest form, builds terms using the following rules:

| syntax   | name        | description |
| -------- | ----------- | ----------- |
| *x*      | variable    | A character or string representing a parameter, mathematical, or logical value.            |
| (λ*x.M*) | abstraction | Function definition *M* — a lambda term. The variable *x* becomes bound in the expression. |
| (*M N*)  | application | Applying a function to an argument. *M* and *N* are lambda terms.                          |
