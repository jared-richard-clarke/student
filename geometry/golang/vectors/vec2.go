package vectors

import (
	"didact/geometry/golang/utils"
	"fmt"
	"math"
)

// A two-dimensional quantity that has direction and magnitude. Represented by a two-part array.
type Vec2 [2]float64

// As opposed to operator "==", method "ApproxEq" whether floating-point vector components are approximately equal.
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

// Returns a vector that is the sum of two vectors.
func (v1 Vec2) Add(v2 Vec2) Vec2 {
	for i := range v1 {
		v1[i] += v2[i]
	}
	return v1
}

// Returns a vector that is the difference of two vectors.
func (v1 Vec2) Sub(v2 Vec2) Vec2 {
	for i := range v1 {
		v1[i] -= v2[i]
	}
	return v1
}

// Inverts the signs of the vector components. Rotates vector 180 degrees.
func (v Vec2) Negate() Vec2 {
	for i := range v {
		v[i] = -v[i]
	}
	return v
}

// Returns the sum of a series of two-dimensional vectors.
func (v1 Vec2) Sum(vs ...Vec2) Vec2 {
	accum := v1
	for _, v := range vs {
		for i := range accum {
			accum[i] += v[i]
		}
	}
	return accum
}

// Computes the distance of a 2d-vector's point from the origin.
func (v Vec2) Mag() float64 {
	return math.Hypot(v[0], v[1])
}

// Scales a 2d-vector by a scalar of s.
func (v Vec2) Scale(s float64) Vec2 {
	for i := range v {
		v[i] *= s
	}
	return v
}

// Computes the dot product of two 2d-vectors.
func (v1 Vec2) Dot(v2 Vec2) float64 {
	id := 0.0
	for i := range v1 {
		id += v1[i] * v2[i]
	}
	return id
}

// Calculates the distance between two vector points.
func (v1 Vec2) Distance(v2 Vec2) float64 {
	for i := range v1 {
		v1[i] = v2[i] - v1[i]
	}
	return math.Hypot(v1[0], v1[1])
}

// Interpolates point between two vector points.
func (v1 Vec2) Lerp(v2 Vec2, t float64) Vec2 {
	for i := range v1 {
		v1[i] = v1[i] + (v2[i]-v1[i])*t
	}
	return v1
}

// Returns a unit vector of the receiver vector.
func (v Vec2) Normalize() Vec2 {
	mag := v.Mag()
	for i := range v {
		v[i] /= mag
	}
	return v
}

// Returns a vector with the rounded components of the receiver vector.
func (v Vec2) Round() Vec2 {
	for i := range v {
		v[i] = math.Round(v[i])
	}
	return v
}

// Fulfills the Stringer interface for the fmt package.
func (v Vec2) String() string {
	x := v[0]
	y := v[1]
	return fmt.Sprintf("vec(%.2f, %.2f)", x, y)
}
