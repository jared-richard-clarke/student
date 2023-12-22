// Provides affine transformation matrices and functions.
package matrices

import (
	utils "didact/geometry/golang/utilities"
	"math"
)

// A 3 × 3 affine transformation matrix. Parameters are described in
// column-major order. Constants are implied.
type Mat3 struct{ A, B, C, D, E, F float64 }

// As opposed to operator "==", method "ApproxEq" checks whether
// floating-point matrix components are approximately equal.
func (m Mat3) ApproxEq(n Mat3) bool {
	eq := utils.ApproxEq
	return eq(m.A, n.A) && eq(m.B, n.B) && eq(m.C, n.C) && eq(m.D, n.D) && eq(m.E, n.E) && eq(m.F, n.F)
}

// Combines matrix transformations through multiplication.
func (m Mat3) Multiply(n Mat3) Mat3 {
	return Mat3{
		m.A*n.A + m.B*n.C,
		m.A*n.B + m.B*n.D,
		m.C*n.A + m.D*n.C,
		m.C*n.B + m.D*n.D,
		m.E*n.A + m.F*n.C + n.E,
		m.E*n.B + m.F*n.D + n.F,
	}
}

// Creates an identity matrix.
func Identity() Mat3 {
	return Mat3{1, 0, 0, 1, 0, 0}
}

// Creates a translation matrix.
func Translate(x, y float64) Mat3 {
	return Mat3{1, 0, 0, 1, x, y}
}

// Creates a scaling matrix.
func Scale(x, y float64) Mat3 {
	return Mat3{x, 0, 0, y, 0, 0}
}

// Creates a rotation matrix.
func Rotate(angle float64) Mat3 {
	c := math.Cos(angle)
	s := math.Sin(angle)
	return Mat3{c, s, -s, c, 0, 0}
}

// Creates a shearing matrix.
func Shear(x, y float64) Mat3 {
	return Mat3{1, y, x, 1, 0, 0}
}

// Translates matrix by scalars "x" and "y". Transformation can be chained.
func (m Mat3) Translate(x, y float64) Mat3 {
	return Translate(x, y).Multiply(m)
}

// Scales matrix by scalars "x" and "y". Transformation can be chained.
func (m Mat3) Scale(x, y float64) Mat3 {
	return Scale(x, y).Multiply(m)
}

// Rotates matrix by "angle", measured in radians. Transformation can be chained.
func (m Mat3) Rotate(angle float64) Mat3 {
	return Rotate(angle).Multiply(m)
}

// Shears matrix by scalars "x" and "y". Transformation can be chained.
func (m Mat3) Shear(x, y float64) Mat3 {
	return Shear(x, y).Multiply(m)
}
