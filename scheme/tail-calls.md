# Tail Call
A call is in tail position with respect to a function if its value is returned directly. 
Nothing is left to do after the call but to return from the function.

## Examples
`func-x` is in the tail position, whereas `func-y` is not.
```scheme
(lambda () (func-x (func-y)))
(lambda () (if (func-y) (func-x) (func-x)))
(lambda () (let [[x 7]] (func-x)))
(lambda () (or (func-y) (func-x)))
```
