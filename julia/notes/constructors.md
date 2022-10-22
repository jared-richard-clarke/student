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

## `constructors.jl`

> "It only makes sense to take ratios of integer values, so the parameter type `T` is 
> restricted to being a subtype of `Integer`, and a ratio of integers represents a value 
> on the real number line, so any `Rational` is an instance of the `Real` abstraction."
>
> — **Julia Manual**

```julia
module Example
"""
Example pulled from [Case Study: Rational](https://docs.julialang.org/en/v1/manual/constructors/)
from Julia's manual documentation on Constructors. Example originally named "OurRational".
"""

struct Rational{T<:Integer} <: Real
    num::T
    den::T
    # ensures rational's components are in normalized form.
    function Rational{T}(num::T, den::T) where {T<:Integer}
        if num == 0 && den == 0
            error("invalid rational: 0//0")
        end
        num = flipsign(num, den)
        den = flipsign(den, den)
        g = gcd(num, den)
        num = div(num, g)
        den = div(den, g)
        new(num, den)
    end
end

"""
"[T]he "standard" general constructor that infers the type parameter T from the type 
of the numerator and denominator when they have the same type." — Julia Manual
"""
Rational(n::T, d::T) where {T<:Integer} = Rational{T}(n, d)

"""
"[Constructor] applies when the given numerator and denominator values have different types: 
it promotes them to a common type and then delegates construction to the outer constructor 
for arguments of matching type." — Julia Manual
"""
Rational(n::Integer, d::Integer) = Rational(promote(n, d)...)

"""
"[C]onstructor turns integer values into rationals by supplying a value of 1 as the denominator." — Julia Manual
"""
Rational(n::Integer) = Rational(n, one(n))

"""
"[P]rovides a syntax for writing rationals (e.g. 1 ⊘ 2)." — Julia Manual
"""
⊘(n::Integer, d::Integer) = Rational(n, d)

"""
The following two methods implement the behavior of a dividing a rational by an integer.
"""
⊘(x::Rational, y::Integer) = x.num ⊘ (x.den * y)
⊘(x::Integer, y::Rational) = (x * y.den) ⊘ y.num

"""
"[A]pplying ⊘ to complex integral values creates an instance of Complex{<:OurRational} 
– a complex number whose real and imaginary parts are rationals[.]" - Julia Manual
"""
⊘(x::Complex, y::Real) = complex(real(x) ⊘ y, imag(x) ⊘ y)

⊘(x::Real, y::Complex) = (x * y') ⊘ real(y * y')

function ⊘(x::Complex, y::Complex)
    xy = x * y'
    yy = real(y * y')
    complex(real(xy) ⊘ yy, imag(xy) ⊘ yy)
end


end # module Example
```
