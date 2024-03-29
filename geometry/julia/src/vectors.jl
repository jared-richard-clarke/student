import Base: +, -, *, abs

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
(+)(v) = v

"""Computes the difference of two vectors."""
sub(v1::Vec2, v2::Vec2) = Vec2(v1.x - v2.x, v1.y - v2.y)
sub(v1::Vec3, v2::Vec3) = Vec3(v1.x - v2.x, v1.y - v2.y, v1.z - v2.z)
(-)(v1, v2) = sub(v1, v2)

"""Flips the signs of the vector components."""
neg(v::Vec2) = Vec2(-v.x, -v.y)
neg(v::Vec3) = Vec3(-v.x, -v.y, -v.z)
(-)(v) = neg(v)

"""Computes a vector with the absolutes of each component."""
abs(v::Vec2) = Vec2(abs(v.x), abs(v.y))
abs(v::Vec3) = Vec3(abs(v.x), abs(v.y), abs(v.z))

"""Inverts the vector components."""
invert(v::Vec2) = Vec2(1 / v.x, 1 / v.y)
invert(v::Vec3) = Vec3(1 / v.x, 1 / v.y, 1 / v.z)

"""Computes the magnitude (a.k.a. length) of a vector."""
mag(v::Vec2) = hypot(v.x, v.y)
mag(v::Vec3) = hypot(v.x, v.y, v.z)

"""Scales a vector by a number."""
scale(v::Vec2, scalar::Real) = Vec2(v.x * scalar, v.y * scalar)
scale(v::Vec3, scalar::Real) = Vec3(v.x * scalar, v.y * scalar, v.z * scalar)
(*)(v, scalar) = scale(v, scalar)
(*)(scalar, v) = scale(v, scalar)

"""Computes the dot product of two vectors."""
dot(v1::Vec2, v2::Vec2) = (v1.x * v2.x) + (v1.y * v2.y)
dot(v1::Vec3, v2::Vec3) = (v1.x * v2.x) + (v1.y * v2.y) + (v1.z * v2.z)

"""Computes the distance between the tips of two vectors."""
distance(v1::Vec2, v2::Vec2) = hypot(v2.x - v1.x, v2.y - v1.y)
distance(v1::Vec3, v2::Vec3) = hypot(v2.x - v1.x, v2.y - v1.y, v2.z - v1.z)

"""Interpolates the components of two vectors."""
lerp(v1::Vec2, v2::Vec2, t::Float64) = v1 + scale(v2 - v1, t)
lerp(v1::Vec3, v2::Vec3, t::Float64) = v1 + scale(v2 - v1, t)

"""Computes the unit vector of a vector."""
function normalize(v::Vec2)
    m = mag(v)
    Vec2(v.x / m, v.y / m)
end

function normalize(v::Vec3)
    m = mag(v)
    Vec3(v.x / m, v.y / m, v.z / m)
end
