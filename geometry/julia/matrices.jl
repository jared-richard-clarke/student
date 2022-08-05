module Matrices

import Base: *

"""
A 2d transformation implemented as a column-major, 3x3 matrix.
The third row is implicit. Columns and rows inverted as in OpenGL.
"""
struct Mat3
    xx::Float64
    yx::Float64
    xy::Float64
    yy::Float64
    x0::Float64
    y0::Float64
end

"""Combines matrix transformations through multiplication."""
function multiply(a::Mat3, b::Mat3)
    Mat3(
        a.xx * b.xx + a.yx * b.xy,
        a.xx * b.yx + a.yx * b.yy,
        a.xy * b.xx + a.yy * b.xy,
        a.xy * b.yx + a.yy * b.yy,
        a.x0 * b.xx + a.y0 * b.xy + b.x0,
        a.x0 * b.yx + a.y0 * b.yy + b.y0
    )
end

"""Matrix multiplication defined as operator."""
(*)(a::Mat3, b::Mat3) = multiply(a, b)

"""Creates a 3x3 identity matrix."""
function identity()
    Mat3(1, 0, 0, 1, 0, 0)
end

"""Creates a 3x3 translation matrix."""
function translate(x::Float64, y::Float64)
    Mat3(1, 0, 0, 1, x, y)
end

"""Creates a 3x3 scaling matrix."""
function scale(x::Float64, y::Float64)
    Mat3(x, 0, 0, y, 0, 0)
end

"""Creates a 3x3 rotation matrix."""
function rotate(angle::Float64)
    c = cos(angle)
    s = sin(angle)
    Mat3(c, s, -s, c, 0, 0)
end

"""Creates a 3x3 shearing matrix."""
function shear(x::Float64, y::Float64)
    Mat3(1, y, x, 1, 0, 0)
end

"""
Multiplies a collection of 3x3 transformation matrices pairwise 
to create a combined transform. Initial value is an identity matrix.
"""
function transform(matrices::Mat3...)
    foldl(multiply, matrices; init=identity())
end

end # module Matrices
