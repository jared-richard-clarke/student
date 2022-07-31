# Parametric Types

[Types: The Julia Language](https://docs.julialang.org/en/v1/manual/types/#)

Below is the definition of Julia's immutable `Rational` number type, representing
an exact ratio of integers. The constructor is omitted.

```julia
struct Rational{T<:Integer} <: Real
    num::T
    den::T
end
```

Type `T` is restricted to being a subtype of `Integer`. A ratio of integers represents
a value on the real number line, therefore `Rational` is an instance of `Real`.
