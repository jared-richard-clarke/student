module VectorGraph

export Vec2, Vec3, abs, invert, mag, scale, dot, distance, lerp, normalize
export Mat3, MAT3_ID, translate, scale, rotate, shear, compose, transform

include("vectors.jl")
include("matrices.jl")

end # module VectorGraph
