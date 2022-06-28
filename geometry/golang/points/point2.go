package points

import "math"

type Point2 struct {
	X, Y float64
}

// Computes the distance between two points.
func (p1 Point2) Distance(p2 Point2) float64 {
	x1 := p1.X
	y1 := p1.Y
	x2 := p2.X
	y2 := p2.Y
	return math.Hypot(x1-x2, y1-y2)
}

// Interpolates a point between two points.
func (p1 Point2) Lerp(p2 Point2, t float64) Point2 {
	x1 := p1.X
	y1 := p1.Y
	x2 := p2.X
	y2 := p2.Y
	x := x1 + (x2-x1)*t
	y := y1 + (y2-y1)*t
	return Point2{x, y}
}
