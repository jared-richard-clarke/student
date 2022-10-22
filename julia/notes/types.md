# Julia: Types

[Types: The Julia Language](https://docs.julialang.org/en/v1/manual/types/#)

## Type System: Dynamic, Nominative, and Parametric

Julia's type system is dynamic, although explicit type annotations can be added.
These annotations serve three purposes:

1. Exploit Julia's multiple-dispatch mechanism.
2. Improve human readability.
3. Catch programmer errors.

> "Julia's type system is designed to be powerful and expressive, yet clear, intuitive 
> and unobtrusive. Many Julia programmers may never feel the need to write code that explicitly
> uses types. Some kinds of programming, however, become clearer, simpler, faster 
> and more robust with declared types." — **Julia Manual**

## Abstract Types

- Cannot be instantiated.
- Serve only as nodes in the type graph, describing sets of related concrete types.
- Form the conceptual hierarchy that makes Julia's type system more than a collection of object implementations.
- `Any` is the supertype for all object instances.
- No object is an instance of `Union{}`. All types are its supertype.

### Syntax

```
abstract type «name» end
abstract type «name» <: «supertype» end
```

## Primitive Types

A primitive type is a concrete type whose data consists of bits. The standard primitive
types are defined in Julia itself: integers, floats, characters, etc.

### Syntax

```
primitive type «name» «bits» end
primitive type «name» <: «supertype» «bits» end
```

## Composite Types

A composite type is a collection of named fields, an instance that can be treated 
as a single value. Julia's composite type is the `struct`.

```julia
# definition
struct Vector
    x::Real
    y::Real
end
# application
v = Vector(3, 4)
```

## Parametric Types

> "An important and powerful feature of Julia's type system is that it is parametric: 
> types can take parameters, so that type declarations actually introduce a whole family 
> of new types – one for each possible combination of parameter values." — **Julia Manual**

Below is the definition of Julia's immutable `Rational` number type, representing
an exact ratio of integers. The constructor is omitted for brevity.

```julia
struct Rational{T<:Integer} <: Real
    num::T
    den::T
end
```

Type `T` is restricted to being a subtype of `Integer`. A ratio of integers represents
a value on the real number line, therefore `Rational` is an instance of `Real`.

## `Union{T, Nothing}`

> A particularly useful case of a Union type is `Union{T, Nothing}`, 
> where `T` can be any type and `Nothing` is the singleton type whose 
> only instance is the object `nothing`. This pattern is the Julia 
> equivalent of `Nullable`, `Option` or `Maybe` types in other languages.
> 
> — **Julia Manual**
