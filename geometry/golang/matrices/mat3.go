// Provides affine transformation matrices and functions.
package matrices

import (
	"didact/geometry/golang/utils"
	"didact/geometry/golang/vectors"
	"math"
)

// A 3 × 3 affine transformation matrix implemented as a 6-part array.
// Parameters are described in column-major order. Constants are implied.
type Mat3 [6]float64

// As opposed to operator "==", method "ApproxEq" checks whether floating-point matrix components are approximately equal.
// Check "didact/geometry/golang/utils/approx-eq.go" for details.
func (m Mat3) ApproxEq(n Mat3) bool {
	eq := utils.ApproxEq
	for i := range m {
		if !eq(m[i], n[i]) {
			return false
		}
	}
	return true
}

// Combines matrix transformations through multiplication.
func (m Mat3) Multiply(n Mat3) Mat3 {
	return Mat3{
		m[0]*n[0] + m[1]*n[2],
		m[0]*n[1] + m[1]*n[3],
		m[2]*n[0] + m[3]*n[2],
		m[2]*n[1] + m[3]*n[3],
		m[4]*n[0] + m[5]*n[2] + n[4],
		m[4]*n[1] + m[5]*n[3] + n[5],
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

func (m Mat3) TransformVector(v vectors.Vec2) vectors.Vec2 {
	return vectors.Vec2{
		m[0]*v[0] + m[2]*v[1],
		m[1]*v[0] + m[3]*v[1],
	}
}
