// Provides affine transformation matrices and functions.
package matrices

import (
	"didact/geometry/golang/utils"
	"math"
)

// A 2d transformation implemented as a column-major, 3 × 3 matrix.
// The third row is implicit.
type Mat3 struct {
	XX, YX,
	XY, YY,
	X0, Y0 float64
}

// Creates an identity matrix.
func Identity() Mat3 {
	return Mat3{
		1, 0,
		0, 1,
		0, 0,
	}
}

// Creates a translation matrix.
func Translate(x, y float64) Mat3 {
	return Mat3{
		1, 0,
		0, 1,
		x, y,
	}
}

// Creates a scaling matrix.
func Scale(x, y float64) Mat3 {
	return Mat3{
		x, 0,
		0, y,
		0, 0,
	}
}

// Creates a rotation matrix.
func Rotate(angle float64) Mat3 {
	c := math.Cos(angle)
	s := math.Sin(angle)
	return Mat3{
		c, s,
		-s, c,
		0, 0,
	}
}

// Creates a shearing matrix.
func Shear(x, y float64) Mat3 {
	return Mat3{
		1, y,
		x, 1,
		0, 0,
	}
}

// Combines matrix transformations through multiplication.
func (a Mat3) Multiply(b Mat3) Mat3 {
	return Mat3{
		a.XX*b.XX + a.YX*b.XY,
		a.XX*b.YX + a.YX*b.YY,
		a.XY*b.XX + a.YY*b.XY,
		a.XY*b.YX + a.YY*b.YY,
		a.X0*b.XX + a.Y0*b.XY + b.X0,
		a.X0*b.YX + a.Y0*b.YY + b.Y0,
	}
}

// Translates matrix by scalars "x" and "y". Transformation can be chained.
func (a Mat3) Translate(x, y float64) Mat3 {
	return Translate(x, y).Multiply(a)
}

// Scales matrix by scalars "x" and "y". Transformation can be chained.
func (a Mat3) Scale(x, y float64) Mat3 {
	return Scale(x, y).Multiply(a)
}

// Rotates matrix by "angle", measured in radians. Transformation can be chained.
func (a Mat3) Rotate(angle float64) Mat3 {
	return Rotate(angle).Multiply(a)
}

// Shears matrix by scalars "x" and "y". Transformation can be chained.
func (a Mat3) Shear(x, y float64) Mat3 {
	return Shear(x, y).Multiply(a)
}

// As opposed to operator "==", method "Equals" checks whether floating-point matrix components are approximately equal.
// Check "didact/geometry/golang/utils/approx-eq.go" for details.
func (a Mat3) Equals(b Mat3) bool {
	eq := utils.ApproxEq
	return eq(a.XX, b.XX) &&
		eq(a.YX, b.YX) &&
		eq(a.XY, b.XY) &&
		eq(a.YY, b.YY) &&
		eq(a.X0, b.X0) &&
		eq(a.Y0, b.Y0)
}
