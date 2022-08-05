module Vectors

import Base: +, -, *

"""
A cartesian representation of a vector in two dimensions.
Has a direction and magnitude.
"""
struct Vec2
    x::Float64
    y::Float64
end

"""Computes the sum of two vectors."""
add(v1::Vec2, v2::Vec2) = Vec2(v1.x + v2.x, v1.y + v2.y)
(+)(v1, v2) = add(v1, v2)

"""Computes the difference of two vectors."""
sub(v1::Vec2, v2::Vec2) = Vec2(v1.x - v2.x, v1.y - v2.y)
(-)(v1, v2) = sub(v1, v2)

"""Inverts the signs of the vector components. Rotates vector 180 degrees."""
neg(v::Vec2) = Vec2(-v.x, -v.y)
(-)(v) = neg(v)

# Returns function that runs a pairwise operation over a sequence.
function total(op, zero)
    (vs) -> length(vs == 0) ? zero : foldl(op, vs; init=zero)
end

"""Computes the sum of a series of vectors."""
sum = total(add, Vec2(0, 0))

"""Computes the difference of a series of vectors."""
diff = total(sub, Vec2(0, 0))

"""Computes the magnitude (a.k.a. length) of a vector."""
mag(v::Vec2) = hypot(v.x, v.y)

"""Scales a vector by a number."""
scale(v::Vec2, scalar::Float64) = Vec2(v.x * scalar, v.y * scalar)
(*)(v, scalar) = scale(v, scalar)
(*)(scalar, v) = scale(v, scalar)

"""Computes the dot product of two vectors."""
dot(v1::Vec2, v2::Vec2) = (v1.x * v2.x) + (v1.y * v2.y)

"""Computes the distance between the tips of two vectors."""
distance(v1::Vec2, v2::Vec2) = hypot(v2.x - v1.x, v2.y - v1.y)

"""Interpolates a vector point along a line between two vector points."""
function lerp(v1::Vec2, v2::Vec2, t::Real)
    x = v1.x + (v2.x - v1.x) * t
    y = v1.y + (v2.y - v1.y) * t
    Vec2(x, y)
end

"""Computes the unit vector of a vector."""
function normalize(v::Vec2)
    m = mag(v)
    x = v.x
    y = v.y
    Vec2(x / m, y / m)
end

# TODO: Will add rounding method after I better understand the rounding mechanism in Julia.
# Rounding is more complex than I originally thought.

end # Vectors module
