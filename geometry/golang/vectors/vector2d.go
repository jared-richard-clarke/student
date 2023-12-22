package vectors

import (
	"didact/geometry/golang/matrices"
	utils "didact/geometry/golang/utilities"
	"fmt"
	"math"
)

// A two-dimensional cartesian Vec2.
type Vec2 struct{ X, Y float64 }

// As opposed to operator "==", function "AboutEqual" tests whether
// floating-point, 2D Vec2 components are approximately equal.
func (v1 Vec2) AboutEqual(v2 Vec2) bool {
	eq := utils.ApproxEq
	x1 := v1.X
	y1 := v1.Y
	x2 := v2.X
	y2 := v2.Y
	return eq(x1, x2) && eq(y1, y2)
}

// Returns a 2D Vec2 with the absolute values of its components.
func (v Vec2) Absolute() Vec2 {
	x := math.Abs(v.X)
	y := math.Abs(v.Y)
	return Vec2{x, y}
}

// Returns a 2D Vec2 that is the sum of two 2D Vec2s.
func (v1 Vec2) Add(v2 Vec2) Vec2 {
	x1 := v1.X
	y1 := v1.Y
	x2 := v2.X
	y2 := v2.Y
	return Vec2{x1 + x2, y1 + y2}
}

// Returns a 2D Vec2 that is the difference of two 2D Vec2s.
func (v1 Vec2) Subtract(v2 Vec2) Vec2 {
	x1 := v1.X
	y1 := v1.Y
	x2 := v2.X
	y2 := v2.Y
	return Vec2{x1 - x2, y1 - y2}
}

// Flips the signs of the 2D Vec2 components.
func (v Vec2) Negate() Vec2 {
	x := v.X
	y := v.Y
	return Vec2{-x, -y}
}

// Inverts the 2D Vec2 components.
func (v Vec2) Invert() Vec2 {
	x := v.X
	y := v.Y
	return Vec2{1.0 / x, 1.0 / y}
}

// Returns the magnitude of a 2D Vec2.
func (v Vec2) Magnitude() float64 {
	x := v.X
	y := v.Y
	return math.Hypot(x, y)
}

// Multiplies a 2D Vec2 by a scalar.
func (v Vec2) Scale(s float64) Vec2 {
	x := v.X
	y := v.Y
	return Vec2{x * s, y * s}
}

// Computes the dot product of two 2D Vec2s.
func (v1 Vec2) DotProduct(v2 Vec2) float64 {
	x1 := v1.X
	y1 := v1.Y
	x2 := v2.X
	y2 := v2.Y
	return x1*x2 + y1*y2
}

// Calculates the distance between two 2D Vec2 points.
func (v1 Vec2) Distance(v2 Vec2) float64 {
	return v2.Subtract(v1).Magnitude()
}

// Interpolates the components of the two 2D Vec2s.
func (v1 Vec2) Interpolate(t float64, v2 Vec2) Vec2 {
	return v1.Add(v2.Subtract(v1).Scale(t))
}

// Returns the unit Vec2 of a 2D Vec2.
func (v Vec2) Normalize() Vec2 {
	m := v.Magnitude()
	x := v.X
	y := v.Y
	return Vec2{x / m, y / m}
}

// Returns a 2D Vec2 with its components rounded.
func (v Vec2) Round() Vec2 {
	x := math.Round(v.X)
	y := math.Round(v.Y)
	return Vec2{x, y}
}

func (v Vec2) TransformBy(m matrices.Mat3) Vec2 {
	return Vec2{
		m.A*v.X + m.C*v.Y,
		m.B*v.X + m.D*v.Y,
	}
}

// Fulfills the Stringer interface for Vec2.
func (v Vec2) String() string {
	x := v.X
	y := v.Y
	return fmt.Sprintf("Vec2(%v, %v)", x, y)
}
