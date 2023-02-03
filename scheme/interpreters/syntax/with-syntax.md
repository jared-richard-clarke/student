# `with-syntax`

Similar to a `let` expression but for syntax pattern matching.

```text
(syntax-case <syntax> () [<pattern> <body>])
(with-syntax ([<pattern> <syntax>]) <body>)
```
