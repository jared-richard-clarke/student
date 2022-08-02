module Points

"""A cartesian representation of a point in two dimensions."""
struct Pt2
    x::Float64
    y::Float64
end

"""Computes the distance between two points."""
function distance(p1::Pt2, p2::Pt2)
    x1 = p1.x
    y1 = p1.y
    x2 = p2.x
    y2 = p2.y
    hypot(x2 - x1, y2 - y1)
end

"""Interpolates a point along a line between two points."""
function lerp(p1::Pt2, p2::Pt2, t::Float64)
    x1 = p1.x
    y1 = p1.y
    x2 = p2.x
    y2 = p2.y
    x = x1 + (x2 - x1) * t
    y = y1 + (y2 - y1) * t
    Pt2(x, y)
end

end # module Points
