# Church Encoding of Boolean Values

The encoding of `true` and `false` are functions of two parameters `a` and `b`,
where `true` returns `a` and `false` returns `b`.

```
true  ≡ λa.λb.a
false ≡ λa.λb.b
```

`true` and `false` can be combined to create logic operators.

```
and = λp.λq.p q p
or  = λp.λq. p p q
not = λp.λa.λb.p b a
if  = λp.λa.λb.p a b
```
