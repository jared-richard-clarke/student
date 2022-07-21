# F\#: Notational Conventions

- `a`, `b`, and `c` are types.
- `f`, `g`, and `h` are functions.
- `x`, `y`, and `z` are arguments to functions.
- For `x::xs`, `x` is head, and `xs` is tail.

## Example

```fsharp
let (|>) aValue aFunction = aFunction aValue
// instead is ...
let (|>) x f = f x
```
