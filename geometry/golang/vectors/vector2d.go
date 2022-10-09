package vectors

import (
	"didact/geometry/golang/matrices"
	"didact/geometry/golang/utils"
	"fmt"
	"math"
)

// A two-dimensional cartesian vector. Represented by a two-part array.
type Vec2 [2]float64

// As opposed to operator "==", function "ApproxEq" tests whether
// floating-point, 2D vector components are approximately equal.
// Check "didact/geometry/golang/utils/approx-eq.go" for details.
func (v1 Vec2) ApproxEq(v2 Vec2) bool {
	eq := utils.ApproxEq
	for i := range v1 {
		if !eq(v1[i], v2[i]) {
			return false
		}
	}
	return true
}

// Returns a 2D vector with the absolute values of its components.
func (v Vec2) Abs() Vec2 {
	for i := range v {
		v[i] = math.Abs(v[i])
	}
	return v
}

// Returns a 2D vector that is the sum of two 2D vectors.
func (v1 Vec2) Add(v2 Vec2) Vec2 {
	for i := range v1 {
		v1[i] += v2[i]
	}
	return v1
}

// Returns a 2D vector that is the difference of two 2D vectors.
func (v1 Vec2) Sub(v2 Vec2) Vec2 {
	for i := range v1 {
		v1[i] -= v2[i]
	}
	return v1
}

// Flips the signs of the 2D vector components.
func (v Vec2) Negate() Vec2 {
	for i := range v {
		v[i] = -v[i]
	}
	return v
}

// Inverts the 2D vector components.
func (v Vec2) Invert() Vec2 {
	for i := range v {
		v[i] = 1.0 / v[i]
	}
	return v
}

// Returns the magnitude of a 2D vector.
func (v Vec2) Mag() float64 {
	return math.Hypot(v[0], v[1])
}

// Multiplies a 2D vector by a scalar.
func (v Vec2) Scale(s float64) Vec2 {
	for i := range v {
		v[i] *= s
	}
	return v
}

// Computes the dot product of two 2D vectors.
func (v1 Vec2) Dot(v2 Vec2) float64 {
	s := 0.0
	for i := range v1 {
		s += v1[i] * v2[i]
	}
	return s
}

// Calculates the distance between two 2D vector points.
func (v1 Vec2) Distance(v2 Vec2) float64 {
	return v2.Sub(v1).Mag()
}

// Interpolates the components of the two 2D vectors.
func (v1 Vec2) Lerp(t float64, v2 Vec2) Vec2 {
	return v1.Add(v2.Sub(v1).Scale(t))
}

// Returns the unit vector of a 2D vector.
func (v Vec2) Normalize() Vec2 {
	mag := v.Mag()
	for i := range v {
		v[i] /= mag
	}
	return v
}

// Returns a 2D vector with its components rounded.
func (v Vec2) Round() Vec2 {
	for i := range v {
		v[i] = math.Round(v[i])
	}
	return v
}

func (v Vec2) TransformBy(m matrices.Mat3) Vec2 {
	return Vec2{
		m[0]*v[0] + m[2]*v[1],
		m[1]*v[0] + m[3]*v[1],
	}
}

// Fulfills the Stringer interface for Vec2.
func (v Vec2) String() string {
	x := v[0]
	y := v[1]
	return fmt.Sprintf("vec2(%.2f, %.2f)", x, y)
}
