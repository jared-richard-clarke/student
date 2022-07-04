import Base: +, -, *

"""
A cartesian representation of a vector in two dimensions.
"""
struct Vec2
    x::Float64
    y::Float64
end

(+)(v1::Vec2, v2::Vec2) = Vec2(v1.x + v2.x, v1.y + v2.y)

(-)(v1::Vec2, v2::Vec2) = Vec2(v1.x - v2.x, v1.y - v2.y)

# negates vector
(-)(v::Vec2) = Vec2(-v.x, -v.y)

# scales vector
(*)(v::Vec2, scalar::Real) = Vec2(v.x * scalar, v.y * scalar)
(*)(scalar::Real, v::Vec2) = v * scalar

"""
Computes the dot product of two vectors.
"""
dot(v1::Vec2, v2::Vec2) = (v1.x * v2.x) + (v1.y * v2.y)

"""
Computes the unit vector of a vector.
"""
function normalize(v::Vec2)
    m = mag(v)
    x = v.x
    y = v.y
    Vec2(x / m, y / m)
end

"""
Computes the magnitude (a.k.a. length) of a vector.
"""
mag(v::Vec2) = hypot(v.x, v.y)

"""
Computes the distance between the points of two vectors.
"""
distance(v1::Vec2, v2::Vec2) = hypot(v2.x - v1.x, v2.y - v1.y)

"""
Interpolates a vector point along a line between two vector points.
"""
function lerp(v1::Vec2, v2::Vec2, t::Real)
    x = v1.x + (v2.x - v1.x) * t
    y = v1.y + (v2.y - v1.y) * t
    Vec2(x, y)
end
