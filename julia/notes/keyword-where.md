# `where` Keyword

Source: [Julia Essentials](https://docs.julialang.org/en/v1/base/base/#)

- The `where` keyword creates a type that is an iterated union of other types, 
  over all values of some variable. For example `Vector{T} where T<:Real` 
  includes all Vectors where the element type is some kind of Real number.


- The variable bound defaults to `Any` if it is omitted:
 `Vector{T} where T` is short for `Vector{T} where T<:Any`

- Variables can also have lower bounds:
 `Vector{T} where Int<:T<:Real`
