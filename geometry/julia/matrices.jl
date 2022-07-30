module Mat3

"""Creates a 3×3 identity matrix."""
function identity()
    [1 0 0
     0 1 0
     0 0 1]
end

"""Creates a 3×3 translation matrix."""
function translate(x, y)
    [1 0 x
     0 1 y
     0 0 1]
end

"""Creates a 3×3 scaling matrix."""
function scale(x, y)
    [x 0 0
     0 y 0
     0 0 1]
end

"""Creates a 3×3 rotation matrix."""
function rotate(angle)
    c = cos(angle)
    s = sin(angle)
    [c -s 0
     s c 0
     0 0 1]
end

"""Creates a 3×3 shearing matrix."""
function shear(x, y)
    [1 x 0
     y 1 0
     0 0 1]
end

"""
Multiplies a collection of 3×3 transformation matrices pairwise 
to create a combined transform. Initial value is an identity matrix.
"""
function transform(matrices...)
    foldl(*, matrices; init=identity())
end

end
