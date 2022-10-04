module VectorGraph

export Vec2, Vec3, invert, mag, scale, dot, distance, lerp, normalize
export Mat3, MAT3_ID, translate, scale, rotate, shear, transform

include("vectors.jl")
include("matrices.jl")

end # module VectorGraph
