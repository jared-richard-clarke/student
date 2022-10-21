# Iterator

Sequential iteration is implemented by the iterate function.

## `iterate` signature

`iterate(iter [, state]) -> Union{Nothing, Tuple{Any, Any}}`

## Equivalent Iterators

### `for` Loop

```julia
for i in iter
    # ...
end
```

### `iterate` function

```julia
next = iterate(iter)
while next !== nothing
    (i, state) = next
    # ...
    next = iterate(iter, state)
end
```
