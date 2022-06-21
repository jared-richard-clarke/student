package points

import (
	"fmt"
	"math"
)

// The maximum allowable difference in precision between two floating-point coordinates.
const epsilon = 0.000001

type Point2 struct {
	X, Y float64
}

// Fulfills the Stringer interface for the fmt package.
func (p Point2) String() string {
	x := p.X
	y := p.Y
	return fmt.Sprintf("pt(%.2f, %.2f)", x, y)
}

// Calculates the distance between two points.
func (p1 Point2) Segment(p2 Point2) float64 {
	x1 := p1.X
	y1 := p1.Y
	x2 := p2.X
	y2 := p2.Y
	return math.Hypot(x2-x1, y2-y1)
}

// A geometric path implemented as a series of points.
type Path []Point2

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
func (p Point2) Round() Point2 {
	x := math.Round(p.X)
	y := math.Round(p.Y)
	return Point2{x, y}
}
