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
	mag := v.Mag()
	x := v.X
	y := v.Y
	return Vec2{x / mag, y / mag}
}

// Computes the distance of a 2d-vector's point from the origin.
func (v Vec2) Mag() float64 {
	x := v.X
	y := v.Y
	return math.Hypot(x, y)
}

// Calculates the distance between two vector points.
func (v1 Vec2) Distance(v2 Vec2) float64 {
	x1 := v1.X
	y1 := v1.Y
	x2 := v2.X
	y2 := v2.Y
	return math.Hypot(x2-x1, y2-y1)
}

// Interpolates point between two vector points.
func (v1 Vec2) Lerp(v2 Vec2, t float64) Vec2 {
	x := v1.X + (v2.X-v1.X)*t
	y := v1.Y + (v2.Y-v1.Y)*t
	return Vec2{x, y}
}

// As opposed to operator "==", method "Equals" whether floating-point vector components are approximately equal.
// Check "didact/geometry/golang/utils/approx-eq.go" for details.
func (v1 Vec2) Equals(v2 Vec2) bool {
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

// Returns a vector with the components of the receiver vector rounded up to the nearest integers.
func (v Vec2) Ceil() Vec2 {
	x := math.Ceil(v.X)
	y := math.Ceil(v.Y)
	return Vec2{x, y}
}

// Returns vector with the components of the receiver vector rounded down to the nearest integers.
func (v Vec2) Floor() Vec2 {
	x := math.Floor(v.X)
	y := math.Floor(v.Y)
	return Vec2{x, y}
}
