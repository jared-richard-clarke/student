// Provides affine transformation matrices and functions.
package matrices

import (
	"didact/geometry/golang/types"
	"math"
)

const (
	zero = 0
	one  = 1
)

// A 3 × 3 affine transformation matrix. Parameters are described in
// column-major order. Constants are implied.
type Mat3[T types.Float] struct{ A, B, C, D, E, F T }

// As opposed to operator "==", method "ApproxEq" checks whether floating-point matrix components are approximately equal.
// Check "didact/geometry/golang/utils/approx-eq.go" for details.
func (m Mat3[T]) ApproxEq(n Mat3[T]) bool {
	eq := types.ApproxEq[T]
	return eq(m.A, n.A) && eq(m.B, n.B) && eq(m.C, n.C) && eq(m.D, n.D) && eq(m.E, n.E) && eq(m.F, n.F)
}

// Combines matrix transformations through multiplication.
func (m Mat3[T]) Multiply(n Mat3[T]) Mat3[T] {
	return Mat3[T]{
		m.A*n.A + m.B*n.C,
		m.A*n.B + m.B*n.D,
		m.C*n.A + m.D*n.C,
		m.C*n.B + m.D*n.D,
		m.E*n.A + m.F*n.C + n.E,
		m.E*n.B + m.F*n.D + n.F,
	}
}

// Creates an identity matrix.
func Identity[T types.Float]() Mat3[T] {
	return Mat3[T]{one, zero, zero, one, zero, zero}
}

// Creates a translation matrix.
func Translate[T types.Float](x, y T) Mat3[T] {
	return Mat3[T]{one, zero, zero, one, x, y}
}

// Creates a scaling matrix.
func Scale[T types.Float](x, y T) Mat3[T] {
	return Mat3[T]{x, zero, zero, y, zero, zero}
}

// Creates a rotation matrix.
func Rotate[T types.Float](angle T) Mat3[T] {
	a := float64(angle)
	c := T(math.Cos(a))
	s := T(math.Sin(a))
	return Mat3[T]{c, s, -s, c, zero, zero}
}

// Creates a shearing matrix.
func Shear[T types.Float](x, y T) Mat3[T] {
	return Mat3[T]{one, y, x, one, zero, zero}
}

// Translates matrix by scalars "x" and "y". Transformation can be chained.
func (m Mat3[T]) Translate(x, y T) Mat3[T] {
	return Translate(x, y).Multiply(m)
}

// Scales matrix by scalars "x" and "y". Transformation can be chained.
func (m Mat3[T]) Scale(x, y T) Mat3[T] {
	return Scale(x, y).Multiply(m)
}

// Rotates matrix by "angle", measured in radians. Transformation can be chained.
func (m Mat3[T]) Rotate(angle T) Mat3[T] {
	return Rotate(angle).Multiply(m)
}

// Shears matrix by scalars "x" and "y". Transformation can be chained.
func (m Mat3[T]) Shear(x, y T) Mat3[T] {
	return Shear(x, y).Multiply(m)
}
