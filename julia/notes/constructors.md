# Equivalent Constructors

## Default Constructor

```julia
struct Point
    x::Real
    y::Real
end
```

## Inner Constructor

```julia
struct Point
    x::Real
    y::Real
    Point(x, y) = new(x, y)
end
```
