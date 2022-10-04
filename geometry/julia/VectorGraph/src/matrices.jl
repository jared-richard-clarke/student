import Base: *

"""
A 2d transformation implemented as a column-major, 3x3 matrix.
The third row is implicit. Columns and rows inverted as in OpenGL.
"""
struct Mat3
    a::Float64
    b::Float64
    c::Float64
    d::Float64
    e::Float64
    f::Float64
end

# Matrices are multiplied right to left. Reversing the arguments
# The reverse arrangement of "m" and "n" allows function foldl to 
# iterate over a list of matrices from left to right while 
# multiplying them as if right to left.

function multiply(n::Mat3, m::Mat3)
    Mat3(
        m.a * n.a + m.b * n.c,
        m.a * n.b + m.b * n.d,
        m.c * n.a + m.d * n.c,
        m.c * n.b + m.d * n.d,
        m.e * n.a + m.f * n.c + n.e,
        m.e * n.b + m.f * n.d + n.f
    )
end

# Flip arguments "m" and "n" again for right to left multiplication
# with infix operator (*).

"""Combines matrix transformations through multiplication."""
(*)(m::Mat3, n::Mat3) = multiply(n, m)

"""A 3x3 identity matrix."""
const MAT3_ID = Mat3(1, 0, 0, 1, 0, 0)

"""Creates a 3x3 translation matrix."""
function translate(x::Real, y::Real)
    Mat3(1, 0, 0, 1, x, y)
end

"""Creates a 3x3 scaling matrix."""
function scale(x::Real, y::Real)
    Mat3(x, 0, 0, y, 0, 0)
end

"""Creates a 3x3 rotation matrix."""
function rotate(angle::Real)
    c = cos(angle)
    s = sin(angle)
    Mat3(c, s, -s, c, 0, 0)
end

"""Creates a 3x3 shearing matrix."""
function shear(x::Real, y::Real)
    Mat3(1, y, x, 1, 0, 0)
end

"""
Multiplies a collection of 3x3 transformation matrices pairwise 
to create a combined transform. Initial value is an identity matrix.
"""
function transform(matrices::Mat3...)
    foldl(multiply, matrices; init=MAT3_ID)
end
