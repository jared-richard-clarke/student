package vectors

import (
	"didact/geometry/golang/utils"
	"fmt"
	"math"
)

// A two-dimensional cartesian vector. Represented by a two-part array.
type Vec2 [2]float64

// A three-dimensional cartesian vector. Represented by a three-part array.
type Vec3 [3]float64

type Vector interface {
	Vec2 | Vec3
}

// As opposed to operator "==", function "ApproxEq" tests whether
// floating-point vector components are approximately equal.
// Check "didact/geometry/golang/utils/approx-eq.go" for details.
func ApproxEq[T Vector](v1, v2 T) bool {
	eq := utils.ApproxEq
	for i := 0; i < len(v1); i += 1 {
		if !eq(v1[i], v2[i]) {
			return false
		}
	}
	return true
}

// Returns a vector that is the sum of two vectors.
func Add[T Vector](v1, v2 T) T {
	for i := 0; i < len(v1); i += 1 {
		v1[i] += v2[i]
	}
	return v1
}

// Returns a vector that is the difference of two vectors.
func Sub[T Vector](v1, v2 T) T {
	for i := 0; i < len(v1); i += 1 {
		v1[i] -= v2[i]
	}
	return v1
}

// Flips the signs of the vector components.
func Negate[T Vector](v T) T {
	for i := 0; i < len(v); i += 1 {
		v[i] = -v[i]
	}
	return v
}

// Inverts the vector components.
func Invert[T Vector](v T) T {
	for i := 0; i < len(v); i += 1 {
		v[i] = 1.0 / v[i]
	}
	return v
}

// Returns the sum of a series of vectors.
func Sum[T Vector](vs ...T) T {
	var accum T
	for i := 0; i < len(vs); i += 1 {
		for j := 0; j < len(vs[i]); j += 1 {
			accum[j] += vs[i][j]
		}
	}
	return accum
}

// Computes the distance of a vector's point from the origin.
func Mag[T Vector](v T) float64 {
	accum := 0.0
	for i := 0; i < len(v); i += 1 {
		accum += math.Pow(v[i], 2)
	}
	return math.Sqrt(accum)
}

// Scales a vector by a scalar of s.
func Scale[T Vector](v T, s float64) T {
	for i := 0; i < len(v); i += 1 {
		v[i] *= s
	}
	return v
}

// Computes the dot product of two vectors.
func Dot[T Vector](v1, v2 T) float64 {
	accum := 0.0
	for i := 0; i < len(v1); i += 1 {
		accum += v1[i] * v2[i]
	}
	return accum
}

// Calculates the distance between two vector points.
func Distance[T Vector](v1, v2 T) float64 {
	accum := 0.0
	for i := 0; i < len(v1); i += 1 {
		accum += math.Pow(v2[i]-v1[i], 2)
	}
	return math.Sqrt(accum)
}

// Linearly interpolates each component between two vectors.
func Lerp[T Vector](v1, v2 T, t float64) T {
	for i := 0; i < len(v1); i += 1 {
		v1[i] += (v2[i] - v1[i]) * t
	}
	return v1
}

// Returns a vectors unit vector.
func Normalize[T Vector](v T) T {
	mag := Mag(v)
	for i := 0; i < len(v); i += 1 {
		v[i] /= mag
	}
	return v
}

// Returns a vector with its components rounded.
func Round[T Vector](v T) T {
	for i := 0; i < len(v); i += 1 {
		v[i] = math.Round(v[i])
	}
	return v
}

// Fulfills the Stringer interface for Vec2.
func (v Vec2) String() string {
	x := v[0]
	y := v[1]
	return fmt.Sprintf("vec2(%.2f, %.2f)", x, y)
}

// Fulfills the Stringer interface for Vec3.
func (v Vec3) String() string {
	x := v[0]
	y := v[1]
	z := v[2]
	return fmt.Sprintf("vec3(%.2f, %.2f, %.2f)", x, y, z)
}
