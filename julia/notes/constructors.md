# Constructors

## Outer Constructors

```julia
struct Point
    x::Real
    y::Real
end

# default constructor

Point() = Point(0, 0)

# single-argument constructor

Point(n) = Point(n, n)
```

## Equivalent Constructors

### Default Constructor

```julia
struct Point
    x::Real
    y::Real
end
```

### Inner Constructor

```julia
struct Point
    x::Real
    y::Real
    Point(x, y) = new(x, y)
end
```
