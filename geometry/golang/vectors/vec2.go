package vectors

import (
	"didact/geometry/golang/utils"
	"fmt"
	"math"
)

// A two-dimensional quantity that has direction and magnitude. Represented by coordinates X and Y.
type Vec2 struct {
	X, Y float64
}

// Ihat and Jhat:
// Mutually orthogonal unit vectors, forming the standard basis.
var (
	Ihat = Vec2{1, 0}
	Jhat = Vec2{0, 1}
)

// Fulfills the Stringer interface for the fmt package.
func (v Vec2) String() string {
	x := v.X
	y := v.Y
	return fmt.Sprintf("vec(%.2f, %.2f)", x, y)
}

// Returns a vector that is the sum of two, two-dimensional vectors.
func (v1 Vec2) Add(v2 Vec2) Vec2 {
	x1 := v1.X
	y1 := v1.Y
	x2 := v2.X
	y2 := v2.Y
	return Vec2{x1 + x2, y1 + y2}
}

// Returns a vector that is the difference of two, two-dimensional vectors.
func (v1 Vec2) Sub(v2 Vec2) Vec2 {
	x1 := v1.X
	y1 := v1.Y
	x2 := v2.X
	y2 := v2.Y
	return Vec2{x1 - x2, y1 - y2}
}

// Returns the sum of a series of two-dimensional vectors.
func (v1 Vec2) Sum(vs ...Vec2) Vec2 {
	accum := v1
	for _, v := range vs {
		x1 := accum.X
		y1 := accum.Y
		x2 := v.X
		y2 := v.Y
		accum = Vec2{x1 + x2, y1 + y2}
	}
	return accum
}

// Returns the difference of a series of two-dimensional vectors.
func (v1 Vec2) Diff(vs ...Vec2) Vec2 {
	accum := v1
	for _, v := range vs {
		x1 := accum.X
		y1 := accum.Y
		x2 := v.X
		y2 := v.Y
		accum = Vec2{x1 - x2, y1 - y2}
	}
	return accum
}

// Inverts the signs of the vector components. Rotates vector 180 degrees.
func (v Vec2) Negate() Vec2 {
	x := v.X
	y := v.Y
	return Vec2{-x, -y}
}

// Scales a 2d-vector by a scalar of s.
func (v Vec2) Scale(s float64) Vec2 {
	x := v.X
	y := v.Y
	return Vec2{x * s, y * s}
}

// Computes the dot product of two 2d-vectors.
func (v1 Vec2) Dot(v2 Vec2) float64 {
	x1 := v1.X
	y1 := v1.Y
	x2 := v2.X
	y2 := v2.Y
	return x1*x2 + y1*y2
}

// Returns a unit vector of the receiver vector.
func (v Vec2) Normalize() Vec2 {
	mag := v.Magnitude()
	x := v.X
	y := v.Y
	return Vec2{x / mag, y / mag}
}

// Computes the distance of a 2d-vector's point from the origin.
func (v Vec2) Magnitude() float64 {
	x := v.X
	y := v.Y
	return math.Hypot(x, y)
}

// Returns the distance between the points of two 2d-vectors.
func (v1 Vec2) Distance(v2 Vec2) float64 {
	x1 := v1.X
	y1 := v1.Y
	x2 := v2.X
	y2 := v2.Y
	return math.Hypot(x2-x1, y2-y1)
}

// Checks whether floating-point vector components are approximately equal.
// Comparisons made left to right.
func (v1 Vec2) ApproxEq(v2 Vec2) bool {
	x1 := v1.X
	y1 := v1.Y
	x2 := v2.X
	y2 := v2.Y
	eq := utils.ApproxEq
	return eq(x1, x2) && eq(y1, y2)
}

// Returns a vector with the rounded components of the receiver vector.
func (v Vec2) Round() Vec2 {
	x := math.Round(v.X)
	y := math.Round(v.Y)
	return Vec2{x, y}
}
