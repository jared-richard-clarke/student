package definitions

import (
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

// Computes the distance of a 2d-vector's point from the origin.
func (v Vector2d) Magnitude() float64 {
	x := v.X
	y := v.Y
	return math.Hypot(x, y)
}

// Scales a 2d-vector by a factor of n.
func Scale(v Vector2d, n float64) Vector2d {
	x := v.X
	y := v.Y
	return Vector2d{x * n, y * n}
}

// Returns the sum of a series of two-dimensional vectors.
func Add(vs ...Vector2d) Vector2d {
	sum := Vector2d{0, 0}
	for _, v := range vs {
		x1 := sum.X
		y1 := sum.Y
		x2 := v.X
		y2 := v.Y
		sum = Vector2d{x1 + x2, y1 + y2}
	}
	return sum
}

// Computes the dot product of two 2d-vectors.
func DotProduct(v1, v2 Vector2d) float64 {
	x1 := v1.X
	y1 := v1.Y
	x2 := v2.X
	y2 := v2.Y
	return x1*x2 + y1*y2
}

// Computes the cross product of two 2d-vectors.
func CrossProduct(v1, v2 Vector2d) float64 {
	x1 := v1.X
	y1 := v1.Y
	x2 := v2.X
	y2 := v2.Y
	return x1*y2 - y1*x2
}

// Generates functions for comparing the magnitudes of two 2d-vectors.
func compare(op func(float64, float64) bool) func(Vector2d, Vector2d) bool {
	return func(v1, v2 Vector2d) bool {
		m1 := v1.Magnitude()
		m2 := v2.Magnitude()
		return op(m1, m2)
	}
}

// 2d-Vector comparison functions
var Gt = compare(func(x, y float64) bool { return x > y })
var Lt = compare(func(x, y float64) bool { return x < y })
var Eq = compare(func(x, y float64) bool { return x == y })
