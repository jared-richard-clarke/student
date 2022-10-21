# `do` Keyword

Create an anonymous function and pass it as the first argument to a function call.

## Equivalent Higher-Order Functions

### `do` syntax

```julia
map([1, 2, 3]) do x
    x - 1
end
```

### anonymous function syntax

```julia
map(x -> x - 1, [1, 2, 3])
```
