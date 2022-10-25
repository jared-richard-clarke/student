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

## Type Declaration: the `::` operator

### Value Declaration

The `::` operator is read as "is an instance of" when appended to an expression computing a value.

```julia
# asserts expression must evaluate to a 64-bit, floating-point number.
sum = (1.0 + 2.0)::Float64
```

### Variable Declaration

When appended to a variable on the left-hand side of an assignment, or as part of a local declaration, 
the `::` operator declares the variable to always have the specified type, like a type declaration 
in a statically-typed language such as C.

```julia
# asserts the variable "sum" must contain a 64-bit, floating-point number.
sum::Float64 = 1.0 + 2.0
```

## Abstract Types

- Cannot be instantiated.
- Serve only as nodes in the type graph, describing sets of related concrete types.
- Form the conceptual hierarchy that makes Julia's type system more than a collection 
  of object implementations.
- `Any` is the supertype for all object instances.
- No object is an instance of `Union{}`. All types are its supertype.
- Provides default implementations for concrete types.
- Allows programmers to write generic functions that can later be used as the 
  default method by many combinations of concrete types.

### Syntax

```
abstract type «name» end
abstract type «name» <: «supertype» end
```

### Numerical Hierarchy

Julia's numerical hierarchy as implemented by abstract types.

```julia
abstract type Number end
abstract type Real          <: Number end
abstract type AbstractFloat <: Real end
abstract type Integer       <: Real end
abstract type Signed        <: Integer end
abstract type Unsigned      <: Integer end
```

## Primitive Types

A primitive type is a concrete type whose data consists of bits. The standard primitive
types are defined in Julia itself: integers, floats, characters, etc.

### Syntax

```
primitive type «name» «bits» end
primitive type «name» <: «supertype» «bits» end
```

### Primitive Definitions

Julia's primitives are defined in the language itself.

```julia
primitive type Float16 <: AbstractFloat 16 end
primitive type Float32 <: AbstractFloat 32 end
primitive type Float64 <: AbstractFloat 64 end

primitive type Bool <: Integer 8 end
primitive type Char <: AbstractChar 32 end

primitive type Int8    <: Signed   8 end
primitive type UInt8   <: Unsigned 8 end
primitive type Int16   <: Signed   16 end
primitive type UInt16  <: Unsigned 16 end
primitive type Int32   <: Signed   32 end
primitive type UInt32  <: Unsigned 32 end
primitive type Int64   <: Signed   64 end
primitive type UInt64  <: Unsigned 64 end
primitive type Int128  <: Signed   128 end
primitive type UInt128 <: Unsigned 128 end
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
## Type Unions

A type union is a special abstract type which includes as objects all instances
of any of its argument types, constructed using the special `Union` keyword

### `Union{Some{T}, Nothing}`

`Union{Some{T}, Nothing}` is used to distinguish between the absence
or presence of a value.

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

## Parametric Primitive Types

```julia
# 32-bit system:
primitive type Ptr{T} 32 end

# 64-bit system:
primitive type Ptr{T} 64 end
```
