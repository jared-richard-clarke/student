module Points

"""A cartesian representation of a point in two dimensions."""
struct Pt2
    x::Float64
    y::Float64
end

"""Computes the distance between two points."""
function distance(p1::Pt2, p2::Pt2)
    hypot(p2.x - p1.x, p2.y - p1.y)
end

"""Interpolates a point along a line between two points."""
function lerp(p1::Pt2, p2::Pt2, t::Float64)
    x = p1.x + (p2.x - p1.x) * t
    y = p1.y + (p2.y - p1.y) * t
    Pt2(x, y)
end

end # module Points
