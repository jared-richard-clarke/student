module Vectors

import Base: +, -, *

"""
A cartesian representation of a vector in two dimensions.
"""
struct Vec2
    x::Float64
    y::Float64
end

"""
A cartesian representation of a vector in three dimensions.
"""
struct Vec3
    x::Float64
    y::Float64
    z::Float64
end

"""Computes the sum of two vectors."""
add(v1::Vec2, v2::Vec2) = Vec2(v1.x + v2.x, v1.y + v2.y)
add(v1::Vec3, v2::Vec3) = Vec3(v1.x + v2.x, v1.y + v2.y, v1.z + v2.z)
(+)(v1, v2) = add(v1, v2)

"""Computes the difference of two vectors."""
sub(v1::Vec2, v2::Vec2) = Vec2(v1.x - v2.x, v1.y - v2.y)
sub(v1::Vec3, v2::Vec3) = Vec3(v1.x - v2.x, v1.y - v2.y, v1.z - v2.z)
(-)(v1, v2) = sub(v1, v2)

"""Inverts the signs of the vector components."""
neg(v::Vec2) = Vec2(-v.x, -v.y)
neg(v::Vec3) = Vec3(-v.x, -v.y, -v.z)
(-)(v) = neg(v)

"""Computes the sum of a series of vectors."""
sum(vs::Vec2...) = reduce(+, vs)
sum(vs::Vec3...) = reduce(+, vs)

"""Computes the magnitude (a.k.a. length) of a vector."""
mag(v::Vec2) = hypot(v.x, v.y)

"""Scales a vector by a number."""
scale(v::Vec2, scalar::Float64) = Vec2(v.x * scalar, v.y * scalar)
scale(v::Vec3, scalar::Float64) = Vec3(v.x * scalar, v.y * scalar, v.z * scalar)
(*)(v, scalar) = scale(v, scalar)
(*)(scalar, v) = scale(v, scalar)

"""Computes the dot product of two vectors."""
dot(v1::Vec2, v2::Vec2) = (v1.x * v2.x) + (v1.y * v2.y)
dot(v1::Vec3, v2::Vec3) = (v1.x * v2.x) + (v1.y * v2.y) + (v1.z * v2.z)

"""Computes the distance between the tips of two vectors."""
distance(v1::Vec2, v2::Vec2) = hypot(v2.x - v1.x, v2.y - v1.y)
distance(v1::Vec3, v2::Vec3) = hypot(v2.x - v1.x, v2.y - v1.y, v2.z - v1.z)

"""Interpolates a vector point along a line between two vector points."""
function lerp(v1::Vec2, v2::Vec2, t::Float64)
    x = v1.x + (v2.x - v1.x) * t
    y = v1.y + (v2.y - v1.y) * t
    Vec2(x, y)
end

function lerp(v1::Vec3, v2::Vec3, t::Float64)
    x = v1.x + (v2.x - v1.x) * t
    y = v1.y + (v2.y - v1.y) * t
    z = v1.z + (v2.z - v1.z) * t
    Vec3(x, y, z)
end

"""Computes the unit vector of a vector."""
function normalize(v::Vec2)
    m = mag(v)
    x = v.x
    y = v.y
    Vec2(x / m, y / m)
end

function normalize(v::Vec3)
    m = mag(v)
    x = v.x
    y = v.y
    z = v.z
    Vec3(x / m, y / m, z / m)
end

# TODO: Will add rounding method after I better understand the rounding mechanism in Julia.
# Rounding is more complex than I originally thought.

end # module Vectors
