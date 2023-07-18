# The Y Combinator

Achieves recursion without named functions.

```
Y = λf.(λx.f (x x)) (λx.f (x x))
```
