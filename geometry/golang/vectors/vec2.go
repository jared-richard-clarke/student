package vectors

import (
	"didact/geometry/golang/utils"
	"fmt"
	"math"
)

// A two-dimensional quantity that has direction and magnitude. Represented by coordinates X and Y.
type Vector2d struct {
	X, Y float64
}

// Ihat, Jhat
// Mutually orthogonal unit vectors, forming the standard basis.
var (
	Ihat = Vector2d{1, 0}
	Jhat = Vector2d{0, 1}
)

// Fulfills the Stringer interface for the fmt package.
func (v Vector2d) String() string {
	x := v.X
	y := v.Y
	return fmt.Sprintf("vec(%.2f, %.2f)", x, y)
}

// Inverts the signs of the vector components. Rotates vector 180 degrees.
func (v Vector2d) Flip() Vector2d {
	x := v.X
	y := v.Y
	return Vector2d{-x, -y}
}

// Computes the distance of a 2d-vector's point from the origin.
func (v Vector2d) Magnitude() float64 {
	x := v.X
	y := v.Y
	return math.Hypot(x, y)
}

// Scales a 2d-vector by a scalar of s.
func (v Vector2d) Scale(s float64) Vector2d {
	x := v.X
	y := v.Y
	return Vector2d{x * s, y * s}
}

// Returns the sum of a series of two-dimensional vectors.
func (v1 Vector2d) Add(vs ...Vector2d) Vector2d {
	accum := v1
	for _, v := range vs {
		x1 := accum.X
		y1 := accum.Y
		x2 := v.X
		y2 := v.Y
		accum = Vector2d{x1 + x2, y1 + y2}
	}
	return accum
}

// Computes the dot product of two 2d-vectors.
func (v1 Vector2d) Dot(v2 Vector2d) float64 {
	x1 := v1.X
	y1 := v1.Y
	x2 := v2.X
	y2 := v2.Y
	return x1*x2 + y1*y2
}

// Returns the distance between two 2d-vectors.
func (v1 Vector2d) Distance(v2 Vector2d) float64 {
	x1 := v1.X
	y1 := v1.Y
	x2 := v2.X
	y2 := v2.Y
	return math.Hypot(x2-x1, y2-y1)
}

// Returns a unit vector of the receiver vector.
func (v Vector2d) Normalize() Vector2d {
	mag := v.Magnitude()
	x := v.X
	y := v.Y
	return Vector2d{x / mag, y / mag}
}

// Checks whether floating-point vector components are approximately equal.
// Comparisons made left to right.
func (v1 Vector2d) ApproxEq(v2 Vector2d) bool {
	x1 := v1.X
	y1 := v1.Y
	x2 := v2.X
	y2 := v2.Y
	eq := utils.ApproxEq
	return eq(x1, x2) && eq(y1, y2)
}

// Returns a vector with the rounded components of the receiver vector.
func (v Vector2d) Round() Vector2d {
	x := math.Round(v.X)
	y := math.Round(v.Y)
	return Vector2d{x, y}
}
