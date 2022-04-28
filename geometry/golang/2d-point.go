package point

import (
	"fmt"
	"math"
)

type Point struct {
	X, Y float64
}

// Fulfills the Stringer interface for the fmt package.
func (p Point) String() string {
	x := p.X
	y := p.Y
	return fmt.Sprintf("pt(%.2f, %.2f)", x, y)
}

// Point of origin in a two-dimensional coordinate system.
var Origin = Point{0, 0}

// Calculates the distance between two points.
func (p1 Point) Segment(p2 Point) float64 {
	x1 := p1.X
	y1 := p1.Y
	x2 := p2.X
	y2 := p2.Y
	return math.Hypot(x2-x1, y2-y1)
}

// A geometric path implemented as a series of points.
type Path []Point

func (p Path) Length() float64 {
	sum := float64(0)
	for i := 0; i < (len(p) - 1); i += 1 {
		p1 := p[i]
		p2 := p[i+1]
		sum += p1.Segment(p2)
	}
	return sum
}

// Outputs a point with the rounded components of the receiver point.
func (p Point) Round() Point {
	x := math.Round(p.X)
	y := math.Round(p.Y)
	return Point{x, y}
}
